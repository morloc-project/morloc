//! C ABI for Arrow tables: the C Data Interface bridge that language pools
//! call, validation against a declared morloc column schema, the nexus
//! printers, and JSON-to-table construction. The block layout and the
//! generic reader/writer live in `arrow_shm`.

use std::ffi::{c_char, c_void, CStr};
use std::io::Write;
use std::sync::Arc;

use arrow_array::cast::AsArray;
use arrow_array::ffi::{FFI_ArrowArray, FFI_ArrowSchema};
use arrow_array::types::*;
use arrow_array::{Array, ArrayRef, RecordBatch};
use arrow_schema::{DataType, Field, Schema as ArrowSchema};

use crate::arrow_shm::{self, ArrowShmHeader};
use crate::cschema::CSchema;
use crate::error::{set_errmsg, MorlocError};
use morloc_runtime_types::{PRINT_RESULT_ERR, PRINT_RESULT_OK, PRINT_RESULT_PIPE_CLOSED};
use crate::schema::{Schema, SerialType};
use crate::shm::{self, RelPtr};

pub use crate::arrow_shm::{ARROW_BUFFER_ALIGN, ARROW_SHM_MAGIC, ARROW_SHM_VERSION};

// -- C Data Interface bridge -------------------------------------------------

/// Move a C Data Interface struct array into a fresh SHM block and return
/// its relative pointer. Takes ownership of `array` (its release callback
/// is invoked once the copy is made); `schema` is only read. Returns
/// RELNULL with `errmsg` set on failure.
#[no_mangle]
pub unsafe extern "C" fn arrow_to_shm(
    array: *mut FFI_ArrowArray,
    schema: *const FFI_ArrowSchema,
    errmsg: *mut *mut c_char,
) -> RelPtr {
    arrow_to_shm_typed(array, schema, std::ptr::null(), errmsg)
}

/// As `arrow_to_shm`, additionally bringing the table into agreement with
/// the declared morloc column schema (`declared` may be NULL or a bare
/// table schema, in which case nothing is declared).
#[no_mangle]
pub unsafe extern "C" fn arrow_to_shm_typed(
    array: *mut FFI_ArrowArray,
    schema: *const FFI_ArrowSchema,
    declared: *const CSchema,
    errmsg: *mut *mut c_char,
) -> RelPtr {
    crate::error::guarded(errmsg, shm::RELNULL, || arrow_to_shm_typed_impl(array, schema, declared, errmsg))
}

unsafe fn arrow_to_shm_typed_impl(
    array: *mut FFI_ArrowArray,
    schema: *const FFI_ArrowSchema,
    declared: *const CSchema,
    errmsg: *mut *mut c_char,
) -> RelPtr {
    let declared_rs = if declared.is_null() { None } else { Some(CSchema::to_rust(declared)) };
    // A table this pool received and is returning unchanged is passed
    // through with a fresh reference; the caller's structs are released
    // exactly as they would be after a copy.
    if let Some(rel) = arrow_shm::try_borrow(array, schema, declared_rs.as_ref()) {
        let raw = array as *mut arrow_shm::RawArray;
        if let Some(release) = (*raw).release {
            release(array);
        }
        if stats_enabled() {
            eprintln!("arrow_to_shm: borrowed");
        }
        return rel;
    }
    let batch = match arrow_shm::ffi_to_batch(array, schema) {
        Ok(b) => b,
        Err(e) => {
            set_errmsg(errmsg, &e);
            return shm::RELNULL;
        }
    };
    let before = arrow_copied_bytes();
    match arrow_shm::write_batch(&batch, declared_rs.as_ref()) {
        Ok(r) => {
            if stats_enabled() {
                eprintln!("arrow_to_shm: copied {} bytes", arrow_copied_bytes() - before);
            }
            r
        }
        Err(e) => {
            set_errmsg(errmsg, &e);
            shm::RELNULL
        }
    }
}

/// `MORLOC_ARROW_STATS=1` reports every table write to stderr: the bytes
/// copied, or that the table was passed through.
fn stats_enabled() -> bool {
    static FLAG: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *FLAG.get_or_init(|| std::env::var_os("MORLOC_ARROW_STATS").is_some())
}

/// As `arrow_to_shm_typed` for a C stream interface producer: every batch
/// the stream yields is concatenated into one table. Takes ownership of
/// the stream, which is released once drained.
#[no_mangle]
pub unsafe extern "C" fn arrow_stream_to_shm_typed(
    stream: *mut arrow_array::ffi_stream::FFI_ArrowArrayStream,
    declared: *const CSchema,
    errmsg: *mut *mut c_char,
) -> RelPtr {
    crate::error::guarded(errmsg, shm::RELNULL, || arrow_stream_to_shm_typed_impl(stream, declared, errmsg))
}

unsafe fn arrow_stream_to_shm_typed_impl(
    stream: *mut arrow_array::ffi_stream::FFI_ArrowArrayStream,
    declared: *const CSchema,
    errmsg: *mut *mut c_char,
) -> RelPtr {
    use arrow_array::ffi_stream::ArrowArrayStreamReader;
    if stream.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("NULL arrow stream".into()));
        return shm::RELNULL;
    }
    let reader = match ArrowArrayStreamReader::from_raw(stream) {
        Ok(r) => r,
        Err(e) => {
            set_errmsg(errmsg, &MorlocError::Other(format!("importing arrow stream: {}", e)));
            return shm::RELNULL;
        }
    };
    let schema = arrow_array::RecordBatchReader::schema(&reader);
    let batches: Vec<RecordBatch> = match reader.collect::<Result<Vec<_>, _>>() {
        Ok(v) => v,
        Err(e) => {
            set_errmsg(errmsg, &MorlocError::Other(format!("reading arrow stream: {}", e)));
            return shm::RELNULL;
        }
    };
    let batch = if batches.len() == 1 {
        batches.into_iter().next().unwrap()
    } else if batches.is_empty() {
        RecordBatch::new_empty(schema)
    } else {
        match arrow_select::concat::concat_batches(&schema, &batches) {
            Ok(b) => b,
            Err(e) => {
                set_errmsg(errmsg, &MorlocError::Other(format!("concatenating arrow stream: {}", e)));
                return shm::RELNULL;
            }
        }
    };
    let declared_rs = if declared.is_null() { None } else { Some(CSchema::to_rust(declared)) };
    match arrow_shm::write_batch(&batch, declared_rs.as_ref()) {
        Ok(r) => r,
        Err(e) => {
            set_errmsg(errmsg, &e);
            shm::RELNULL
        }
    }
}

/// Build C Data Interface views over a block. Buffer pointers point into
/// the block, which must outlive the view. Returns 0 on success.
#[no_mangle]
pub unsafe extern "C" fn arrow_from_shm(
    header: *const ArrowShmHeader,
    out_schema: *mut FFI_ArrowSchema,
    out_array: *mut FFI_ArrowArray,
    errmsg: *mut *mut c_char,
) -> i32 {
    crate::error::guarded(errmsg, 1, || arrow_from_shm_impl(header, out_schema, out_array, errmsg))
}

unsafe fn arrow_from_shm_impl(
    header: *const ArrowShmHeader,
    out_schema: *mut FFI_ArrowSchema,
    out_array: *mut FFI_ArrowArray,
    errmsg: *mut *mut c_char,
) -> i32 {
    match arrow_shm::shm_to_ffi(header, out_schema, out_array) {
        Ok(()) => 0,
        Err(e) => {
            set_errmsg(errmsg, &e);
            1
        }
    }
}

/// Check a block against the morloc column schema it is received under.
/// Returns 0 when every declared column is present with an acceptable
/// physical type and nullability, 1 otherwise with `errmsg` set.
#[no_mangle]
pub unsafe extern "C" fn arrow_validate(
    header: *const ArrowShmHeader,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> i32 {
    crate::error::guarded(errmsg, 1, || arrow_validate_impl(header, schema, errmsg))
}

unsafe fn arrow_validate_impl(
    header: *const ArrowShmHeader,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> i32 {
    if schema.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("NULL schema for arrow validation".into()));
        return 1;
    }
    let rs = CSchema::to_rust(schema);
    match arrow_shm::validate(header, &rs) {
        Ok(()) => 0,
        Err(e) => {
            set_errmsg(errmsg, &e);
            1
        }
    }
}

/// As `arrow_from_shm`, with the view holding the block for as long as the
/// language object built from it lives, and no longer. `acquire` takes a
/// reference of the view's own -- for a table that arrived by reference,
/// whose sender still holds one; pass 0 for a block this pool materialised
/// for itself, whose only reference the view adopts. Returns 0 on success;
/// on failure nothing is taken and an adopted reference is still the
/// caller's to release.
#[no_mangle]
pub unsafe extern "C" fn arrow_from_shm_owned(
    header: *const ArrowShmHeader,
    acquire: i32,
    out_schema: *mut FFI_ArrowSchema,
    out_array: *mut FFI_ArrowArray,
    errmsg: *mut *mut c_char,
) -> i32 {
    crate::error::guarded(errmsg, 1, || {
        match arrow_shm::shm_to_ffi_owned(header, acquire != 0, out_schema, out_array) {
            Ok(()) => 0,
            Err(e) => {
                set_errmsg(errmsg, &e);
                1
            }
        }
    })
}

/// Bytes of shared memory held by this process's open table views. A
/// language whose garbage collector cannot see shared memory uses this to
/// decide when a collection is worth running: its own heap accounting puts
/// a batch at a few hundred bytes whatever the table behind it costs.
#[no_mangle]
pub extern "C" fn arrow_live_view_bytes() -> usize {
    arrow_shm::LIVE_VIEW_BYTES.load(std::sync::atomic::Ordering::Relaxed)
}

/// Bytes memcpy'd into SHM by table writes in this process so far.
#[no_mangle]
pub extern "C" fn arrow_copied_bytes() -> u64 {
    arrow_shm::COPIED_BYTES.load(std::sync::atomic::Ordering::Relaxed)
}

// -- Printers ----------------------------------------------------------------

/// C's `%.<prec>g` rendering of a double.
fn fmt_g(v: f64, prec: i32) -> String {
    let mut buf = [0u8; 64];
    let n = unsafe {
        libc::snprintf(
            buf.as_mut_ptr() as *mut c_char,
            buf.len(),
            b"%.*g\0".as_ptr() as *const c_char,
            prec,
            v,
        )
    };
    let n = n.clamp(0, buf.len() as i32 - 1) as usize;
    String::from_utf8_lossy(&buf[..n]).into_owned()
}

fn json_escape(s: &str, out: &mut String) {
    out.push('"');
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 32 => out.push_str(&format!("\\u{:04x}", c as u32)),
            c => out.push(c),
        }
    }
    out.push('"');
}

fn hex(bytes: &[u8], out: &mut String) {
    out.push('"');
    for b in bytes {
        out.push_str(&format!("{:02x}", b));
    }
    out.push('"');
}

/// Render one cell as JSON. Numbers and text render natively; nested
/// lists and structs recurse; anything else renders as the quoted text
/// Arrow's own formatter gives it (timestamps as ISO 8601, and so on).
fn json_cell(col: &dyn Array, i: usize, out: &mut String) {
    if col.is_null(i) {
        out.push_str("null");
        return;
    }
    macro_rules! prim {
        ($t:ty) => {
            out.push_str(&col.as_primitive::<$t>().value(i).to_string())
        };
    }
    match col.data_type() {
        DataType::Boolean => out.push_str(if col.as_boolean().value(i) { "true" } else { "false" }),
        DataType::Int8 => prim!(Int8Type),
        DataType::Int16 => prim!(Int16Type),
        DataType::Int32 => prim!(Int32Type),
        DataType::Int64 => prim!(Int64Type),
        DataType::UInt8 => prim!(UInt8Type),
        DataType::UInt16 => prim!(UInt16Type),
        DataType::UInt32 => prim!(UInt32Type),
        DataType::UInt64 => prim!(UInt64Type),
        DataType::Float32 => out.push_str(&fmt_g(col.as_primitive::<Float32Type>().value(i) as f64, 7)),
        DataType::Float64 => out.push_str(&fmt_g(col.as_primitive::<Float64Type>().value(i), 15)),
        DataType::Utf8 => json_escape(col.as_string::<i32>().value(i), out),
        DataType::LargeUtf8 => json_escape(col.as_string::<i64>().value(i), out),
        DataType::Binary => hex(col.as_binary::<i32>().value(i), out),
        DataType::LargeBinary => hex(col.as_binary::<i64>().value(i), out),
        DataType::List(_) => {
            let inner = col.as_list::<i32>().value(i);
            json_seq(inner.as_ref(), out);
        }
        DataType::LargeList(_) => {
            let inner = col.as_list::<i64>().value(i);
            json_seq(inner.as_ref(), out);
        }
        DataType::Struct(fields) => {
            let s = col.as_struct();
            out.push('{');
            for (j, f) in fields.iter().enumerate() {
                if j > 0 {
                    out.push(',');
                }
                json_escape(f.name(), out);
                out.push(':');
                json_cell(s.column(j).as_ref(), i, out);
            }
            out.push('}');
        }
        _ => {
            let opts = arrow_cast::display::FormatOptions::default();
            match arrow_cast::display::ArrayFormatter::try_new(col, &opts) {
                Ok(f) => json_escape(&f.value(i).to_string(), out),
                Err(_) => out.push_str("null"),
            }
        }
    }
}

fn json_seq(arr: &dyn Array, out: &mut String) {
    out.push('[');
    for k in 0..arr.len() {
        if k > 0 {
            out.push(',');
        }
        json_cell(arr, k, out);
    }
    out.push(']');
}

/// Write a rendered block to stdout with the same result codes as
/// `print_voidstar`: a closed pipe is distinguished from other failures.
unsafe fn write_stdout(s: &str, errmsg: *mut *mut c_char) -> i32 {
    let mut w = std::io::stdout().lock();
    match w.write_all(s.as_bytes()).and_then(|_| w.flush()) {
        Ok(()) => PRINT_RESULT_OK,
        Err(e) if e.kind() == std::io::ErrorKind::BrokenPipe => PRINT_RESULT_PIPE_CLOSED,
        Err(e) => {
            set_errmsg(errmsg, &MorlocError::Serialization(e.to_string()));
            PRINT_RESULT_ERR
        }
    }
}

/// Print a block as a JSON array of row objects, one line.
#[no_mangle]
pub unsafe extern "C" fn print_arrow_as_json(data: *const c_void, errmsg: *mut *mut c_char) -> i32 {
    crate::error::guarded(errmsg, PRINT_RESULT_ERR, || print_arrow_as_json_impl(data, errmsg))
}

unsafe fn print_arrow_as_json_impl(data: *const c_void, errmsg: *mut *mut c_char) -> i32 {
    let batch = match arrow_shm::shm_to_batch(data as *const ArrowShmHeader) {
        Ok(b) => b,
        Err(e) => {
            set_errmsg(errmsg, &e);
            return PRINT_RESULT_ERR;
        }
    };
    let mut out = String::new();
    out.push('[');
    for r in 0..batch.num_rows() {
        if r > 0 {
            out.push(',');
        }
        out.push('{');
        for (c, field) in batch.schema().fields().iter().enumerate() {
            if c > 0 {
                out.push(',');
            }
            json_escape(field.name(), &mut out);
            out.push(':');
            json_cell(batch.column(c).as_ref(), r, &mut out);
        }
        out.push('}');
    }
    out.push_str("]\n");
    write_stdout(&out, errmsg)
}

/// Print a block as a tab-separated table: a header line of column names,
/// then one line per row with cells rendered as in the JSON form.
#[no_mangle]
pub unsafe extern "C" fn print_arrow_as_table(data: *const c_void, errmsg: *mut *mut c_char) -> i32 {
    crate::error::guarded(errmsg, PRINT_RESULT_ERR, || print_arrow_as_table_impl(data, errmsg))
}

unsafe fn print_arrow_as_table_impl(data: *const c_void, errmsg: *mut *mut c_char) -> i32 {
    let batch = match arrow_shm::shm_to_batch(data as *const ArrowShmHeader) {
        Ok(b) => b,
        Err(e) => {
            set_errmsg(errmsg, &e);
            return PRINT_RESULT_ERR;
        }
    };
    let mut out = String::new();
    let schema = batch.schema();
    let names: Vec<String> = schema.fields().iter().map(|f| f.name().to_string()).collect();
    out.push_str(&names.join("\t"));
    out.push('\n');
    for r in 0..batch.num_rows() {
        for c in 0..batch.num_columns() {
            if c > 0 {
                out.push('\t');
            }
            json_cell(batch.column(c).as_ref(), r, &mut out);
        }
        out.push('\n');
    }
    write_stdout(&out, errmsg)
}

// -- Arrow detection / JSON -> table -----------------------------------------

/// True iff the schema represents an Arrow table.
pub fn is_arrow_table_schema(s: &Schema) -> bool {
    s.serial_type == SerialType::Table
}

/// One column's worth of inference state collected from JSON: the first
/// non-null sample (if any) and whether any null was seen.
struct JsonColInfo<'a> {
    sample: Option<&'a serde_json::Value>,
    has_null: bool,
}

/// Walk a JSON value (either row-oriented `[{...}, ...]` or column-
/// oriented `{"col": [...], ...}`) and collect the union of column names
/// in first-seen order, along with a sample value and a null indicator
/// per column.
fn discover_json_columns<'a>(
    value: &'a serde_json::Value,
) -> Result<(Vec<String>, std::collections::HashMap<String, JsonColInfo<'a>>), MorlocError> {
    use serde_json::Value;

    let mut order: Vec<String> = Vec::new();
    let mut info: std::collections::HashMap<String, JsonColInfo<'a>> = std::collections::HashMap::new();

    let see = |k: &str,
               v: &'a Value,
               order: &mut Vec<String>,
               info: &mut std::collections::HashMap<String, JsonColInfo<'a>>| {
        let entry = info.entry(k.to_string()).or_insert_with(|| {
            order.push(k.to_string());
            JsonColInfo { sample: None, has_null: false }
        });
        if v.is_null() {
            entry.has_null = true;
        } else if entry.sample.is_none() {
            entry.sample = Some(v);
        }
    };

    match value {
        Value::Array(rows) => {
            for (i, row) in rows.iter().enumerate() {
                let obj = row
                    .as_object()
                    .ok_or_else(|| MorlocError::Other(format!("Row {} is not a JSON object", i)))?;
                for (k, v) in obj {
                    see(k, v, &mut order, &mut info);
                }
            }
        }
        Value::Object(cols) => {
            for (k, v) in cols {
                let arr = v
                    .as_array()
                    .ok_or_else(|| MorlocError::Other(format!("Column '{}' must be a JSON array", k)))?;
                if arr.is_empty() {
                    info.entry(k.to_string()).or_insert_with(|| {
                        order.push(k.to_string());
                        JsonColInfo { sample: None, has_null: false }
                    });
                }
                for v in arr {
                    see(k, v, &mut order, &mut info);
                }
            }
        }
        _ => {
            return Err(MorlocError::Other(
                "Top-level JSON for an Arrow table must be array (row-oriented) or object (column-oriented)"
                    .into(),
            ))
        }
    }
    Ok((order, info))
}

fn sample_to_serial_type(sample: Option<&serde_json::Value>) -> SerialType {
    use serde_json::Value;
    match sample {
        Some(Value::Bool(_)) => SerialType::Bool,
        Some(Value::Number(n)) if n.is_i64() || n.is_u64() => SerialType::Sint64,
        Some(Value::Number(_)) => SerialType::Float64,
        Some(Value::String(_)) => SerialType::String,
        Some(_) | None => SerialType::String,
    }
}

/// Wrap a primitive 'Schema' in 'SerialType::Optional' if @nullable@.
pub fn maybe_optional(inner: Schema, nullable: bool) -> Schema {
    if !nullable {
        return inner;
    }
    Schema {
        serial_type: SerialType::Optional,
        size: 1,
        width: std::mem::size_of::<RelPtr>(),
        offsets: Vec::new(),
        hint: None,
        parameters: vec![inner],
        keys: Vec::new(),
        name: None,
    }
}

/// Merge the declared morloc Table schema with the columns discovered in
/// a JSON value. Declared columns are authoritative for their types and
/// come first; undeclared columns flow through with inferred types, wrapped
/// in Optional iff the JSON held a null for them.
fn merge_table_schema_with_json(rs: &Schema, value: &serde_json::Value) -> Result<Schema, MorlocError> {
    let (discovered_order, info) = discover_json_columns(value)?;

    for k in &rs.keys {
        if !info.contains_key(k) {
            return Err(MorlocError::Other(format!("Declared column '{}' missing from JSON input", k)));
        }
    }

    let declared: std::collections::HashSet<&str> = rs.keys.iter().map(|s| s.as_str()).collect();
    let mut keys: Vec<String> = Vec::with_capacity(rs.keys.len() + discovered_order.len());
    let mut params: Vec<Schema> = Vec::with_capacity(rs.keys.len() + discovered_order.len());
    for (i, k) in rs.keys.iter().enumerate() {
        keys.push(k.clone());
        params.push(rs.parameters[i].clone());
    }
    for k in discovered_order.iter() {
        if declared.contains(k.as_str()) {
            continue;
        }
        let col_info = info.get(k).expect("discovered key must be in info");
        let inner = Schema::primitive(sample_to_serial_type(col_info.sample));
        keys.push(k.clone());
        params.push(maybe_optional(inner, col_info.has_null));
    }

    Ok(Schema {
        serial_type: SerialType::Table,
        size: keys.len(),
        width: std::mem::size_of::<shm::Array>(),
        offsets: Vec::new(),
        hint: None,
        parameters: params,
        keys,
        name: None,
    })
}

/// Parse JSON text into a fresh table block according to the given Table
/// schema. Accepts both row-oriented and column-oriented forms. Returns a
/// RelPtr to the new block or RELNULL on error.
///
/// # Safety
/// `json` must be a valid null-terminated UTF-8 string and `schema` a
/// valid CSchema pointer.
#[no_mangle]
pub unsafe extern "C" fn read_json_to_arrow_shm(
    json: *const c_char,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> RelPtr {
    crate::error::guarded(errmsg, shm::RELNULL, || read_json_to_arrow_shm_impl(json, schema, errmsg))
}

unsafe fn read_json_to_arrow_shm_impl(
    json: *const c_char,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> RelPtr {
    if json.is_null() || schema.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("NULL json or schema".into()));
        return shm::RELNULL;
    }
    let rs = CSchema::to_rust(schema);
    if !is_arrow_table_schema(&rs) {
        set_errmsg(errmsg, &MorlocError::Other("JSON-to-table requires a Table schema".into()));
        return shm::RELNULL;
    }
    read_json_bytes_to_arrow_shm(CStr::from_ptr(json).to_bytes(), schema, errmsg)
}

/// As `read_json_to_arrow_shm` for JSON that is not already a C string --
/// a file read into memory, say.
///
/// # Safety
/// `schema` must be a valid CSchema pointer.
pub unsafe fn read_json_bytes_to_arrow_shm(
    json: &[u8],
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> RelPtr {
    let rs = CSchema::to_rust(schema);
    if !is_arrow_table_schema(&rs) {
        set_errmsg(errmsg, &MorlocError::Other("JSON-to-table requires a Table schema".into()));
        return shm::RELNULL;
    }
    let value: serde_json::Value = match serde_json::from_slice(json) {
        Ok(v) => v,
        Err(e) => {
            set_errmsg(errmsg, &MorlocError::Other(format!("JSON parse error: {}", e)));
            return shm::RELNULL;
        }
    };
    match json_value_to_batch(&value, &rs).and_then(|b| arrow_shm::write_batch(&b, Some(&rs))) {
        Ok(r) => r,
        Err(e) => {
            set_errmsg(errmsg, &e);
            shm::RELNULL
        }
    }
}

/// Build a record batch from parsed JSON under a Table schema, merging
/// declared and discovered columns as described in
/// `merge_table_schema_with_json`.
pub fn json_value_to_batch(value: &serde_json::Value, rs: &Schema) -> Result<RecordBatch, MorlocError> {
    use serde_json::Value;

    let merged = merge_table_schema_with_json(rs, value)?;
    let n_cols = merged.size;

    let mut columns: Vec<Vec<&Value>> = (0..n_cols).map(|_| Vec::new()).collect();
    match value {
        Value::Array(rows) => {
            for (row_idx, row) in rows.iter().enumerate() {
                let obj = row
                    .as_object()
                    .ok_or_else(|| MorlocError::Other(format!("Row {} is not a JSON object", row_idx)))?;
                for (col_idx, key) in merged.keys.iter().enumerate() {
                    let v = obj.get(key).ok_or_else(|| {
                        MorlocError::Other(format!("Row {} missing column '{}'", row_idx, key))
                    })?;
                    columns[col_idx].push(v);
                }
            }
        }
        Value::Object(obj) => {
            let mut row_count: Option<usize> = None;
            for (col_idx, key) in merged.keys.iter().enumerate() {
                match obj.get(key) {
                    Some(Value::Array(arr)) => {
                        match row_count {
                            None => row_count = Some(arr.len()),
                            Some(n) if n == arr.len() => {}
                            Some(n) => {
                                return Err(MorlocError::Other(format!(
                                    "Column '{}' has {} rows, expected {}",
                                    key,
                                    arr.len(),
                                    n
                                )))
                            }
                        }
                        columns[col_idx] = arr.iter().collect();
                    }
                    _ => return Err(MorlocError::Other(format!("Column '{}' must be a JSON array", key))),
                }
            }
        }
        _ => unreachable!("discover_json_columns rejects other shapes"),
    }

    let mut fields: Vec<Field> = Vec::with_capacity(n_cols);
    let mut arrays: Vec<ArrayRef> = Vec::with_capacity(n_cols);
    for (i, p) in merged.parameters.iter().enumerate() {
        let (inner, nullable) = if p.serial_type == SerialType::Optional {
            (p.parameters.first().map(|c| c.serial_type).unwrap_or(SerialType::Nil), true)
        } else {
            (p.serial_type, false)
        };
        let name = &merged.keys[i];
        let arr = json_column(inner, nullable, &columns[i], name)?;
        fields.push(Field::new(name, arr.data_type().clone(), nullable));
        arrays.push(arr);
    }
    RecordBatch::try_new(Arc::new(ArrowSchema::new(fields)), arrays)
        .map_err(|e| MorlocError::Other(format!("building table from JSON: {}", e)))
}

/// Build one typed Arrow array from JSON cells. A JSON null is a null cell
/// and is accepted only in a nullable column. A number is stored only when
/// the declared width holds it exactly; one that does not fit is an error
/// naming the column and row, never a truncated value.
fn json_column(
    st: SerialType,
    nullable: bool,
    values: &[&serde_json::Value],
    name: &str,
) -> Result<ArrayRef, MorlocError> {
    use arrow_array::*;

    fn bad(name: &str, row: usize, expected: &str) -> MorlocError {
        MorlocError::Other(format!("Expected {} in column '{}' row {}", expected, name, row))
    }
    fn overflow(name: &str, row: usize, v: &serde_json::Value, st: SerialType) -> MorlocError {
        MorlocError::Other(format!("Value {} in column '{}' row {} does not fit the declared {:?}", v, name, row, st))
    }
    /// Every cell as an exact integer, then narrowed by `TryFrom`.
    fn ints<T>(values: &[&serde_json::Value], nullable: bool, name: &str, st: SerialType) -> Result<Vec<Option<T>>, MorlocError>
    where
        T: TryFrom<i64> + TryFrom<u64>,
    {
        values
            .iter()
            .enumerate()
            .map(|(i, v)| {
                if v.is_null() {
                    return if nullable { Ok(None) } else { Err(bad(name, i, "integer")) };
                }
                let fitted = if let Some(x) = v.as_i64() {
                    T::try_from(x).ok()
                } else if let Some(x) = v.as_u64() {
                    T::try_from(x).ok()
                } else {
                    return Err(bad(name, i, "integer"));
                };
                fitted.map(Some).ok_or_else(|| overflow(name, i, v, st))
            })
            .collect()
    }
    fn floats(values: &[&serde_json::Value], nullable: bool, name: &str) -> Result<Vec<Option<f64>>, MorlocError> {
        values
            .iter()
            .enumerate()
            .map(|(i, v)| {
                if v.is_null() {
                    return if nullable { Ok(None) } else { Err(bad(name, i, "number")) };
                }
                v.as_f64().map(Some).ok_or_else(|| bad(name, i, "number"))
            })
            .collect()
    }
    /// A double narrowed to single precision only when the value survives
    /// the round trip (a finite value beyond f32's range would become
    /// infinite; precision loss is the accepted cost of declaring Float32).
    fn float32s(values: &[&serde_json::Value], nullable: bool, name: &str) -> Result<Vec<Option<f32>>, MorlocError> {
        floats(values, nullable, name)?
            .into_iter()
            .enumerate()
            .map(|(i, o)| match o {
                None => Ok(None),
                Some(x) => {
                    let y = x as f32;
                    if x.is_finite() && !y.is_finite() {
                        Err(overflow(name, i, values[i], SerialType::Float32))
                    } else {
                        Ok(Some(y))
                    }
                }
            })
            .collect()
    }

    let arr: ArrayRef = match st {
        SerialType::Bool => {
            let v: Vec<Option<bool>> = values
                .iter()
                .enumerate()
                .map(|(i, v)| {
                    if v.is_null() {
                        return if nullable { Ok(None) } else { Err(bad(name, i, "bool")) };
                    }
                    v.as_bool().map(Some).ok_or_else(|| bad(name, i, "bool"))
                })
                .collect::<Result<_, _>>()?;
            Arc::new(BooleanArray::from(v))
        }
        SerialType::Sint8 => Arc::new(Int8Array::from(ints::<i8>(values, nullable, name, st)?)),
        SerialType::Sint16 => Arc::new(Int16Array::from(ints::<i16>(values, nullable, name, st)?)),
        SerialType::Sint32 => Arc::new(Int32Array::from(ints::<i32>(values, nullable, name, st)?)),
        SerialType::Sint64 | SerialType::Int => Arc::new(Int64Array::from(ints::<i64>(values, nullable, name, st)?)),
        SerialType::Uint8 => Arc::new(UInt8Array::from(ints::<u8>(values, nullable, name, st)?)),
        SerialType::Uint16 => Arc::new(UInt16Array::from(ints::<u16>(values, nullable, name, st)?)),
        SerialType::Uint32 => Arc::new(UInt32Array::from(ints::<u32>(values, nullable, name, st)?)),
        SerialType::Uint64 => Arc::new(UInt64Array::from(ints::<u64>(values, nullable, name, st)?)),
        SerialType::Float32 => Arc::new(Float32Array::from(float32s(values, nullable, name)?)),
        SerialType::Float64 => Arc::new(Float64Array::from(floats(values, nullable, name)?)),
        SerialType::String => {
            let v: Vec<Option<&str>> = values
                .iter()
                .enumerate()
                .map(|(i, v)| {
                    if v.is_null() {
                        return if nullable { Ok(None) } else { Err(bad(name, i, "string")) };
                    }
                    v.as_str().map(Some).ok_or_else(|| bad(name, i, "string"))
                })
                .collect::<Result<_, _>>()?;
            Arc::new(StringArray::from(v))
        }
        other => {
            return Err(MorlocError::Other(format!(
                "Unsupported column type {:?} for '{}' when building a table from JSON",
                other, name
            )))
        }
    };
    Ok(arr)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn table(cols: &[(&str, SerialType)]) -> Schema {
        Schema::table(
            cols.iter().map(|(_, t)| Schema::primitive(*t)).collect(),
            cols.iter().map(|(k, _)| k.to_string()).collect(),
        )
    }

    fn column(st: SerialType, json: &str) -> Result<ArrayRef, MorlocError> {
        let v: serde_json::Value = serde_json::from_str(json).unwrap();
        let rs = table(&[("c", st)]);
        json_value_to_batch(&v, &rs).map(|b| b.column(0).clone())
    }

    #[test]
    fn json_numbers_that_do_not_fit_the_declared_width_are_refused() {
        use arrow_array::*;
        let refused = [
            (SerialType::Uint8, "[{\"c\": 300}]"),
            (SerialType::Uint8, "[{\"c\": -1}]"),
            (SerialType::Sint8, "[{\"c\": 200}]"),
            (SerialType::Sint16, "[{\"c\": 70000}]"),
            (SerialType::Sint32, "[{\"c\": 2147483648}]"),
            (SerialType::Uint32, "[{\"c\": -7}]"),
            (SerialType::Sint64, "[{\"c\": 18446744073709551615}]"),
            (SerialType::Float32, "[{\"c\": 1e300}]"),
        ];
        for (st, json) in refused {
            let e = column(st, json).err().unwrap_or_else(|| panic!("{:?} accepted {}", st, json));
            let msg = format!("{}", e);
            assert!(msg.contains("column 'c' row 0"), "{}", msg);
        }
        // The edges of each width are kept exactly.
        let ok = column(SerialType::Uint8, "[{\"c\": 255}, {\"c\": 0}]").unwrap();
        assert_eq!(ok.as_any().downcast_ref::<UInt8Array>().unwrap().values(), &[255, 0]);
        let ok = column(SerialType::Sint8, "[{\"c\": -128}, {\"c\": 127}]").unwrap();
        assert_eq!(ok.as_any().downcast_ref::<Int8Array>().unwrap().values(), &[-128, 127]);
        let ok = column(SerialType::Uint64, "[{\"c\": 18446744073709551615}]").unwrap();
        assert_eq!(ok.as_any().downcast_ref::<UInt64Array>().unwrap().values(), &[u64::MAX]);
        let ok = column(SerialType::Sint64, "[{\"c\": -9223372036854775808}]").unwrap();
        assert_eq!(ok.as_any().downcast_ref::<Int64Array>().unwrap().values(), &[i64::MIN]);
        let ok = column(SerialType::Float32, "[{\"c\": 0.1}]").unwrap();
        assert_eq!(ok.as_any().downcast_ref::<Float32Array>().unwrap().values(), &[0.1f32]);
    }
}
