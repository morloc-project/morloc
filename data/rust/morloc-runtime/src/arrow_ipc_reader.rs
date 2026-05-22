//! Arrow IPC / Parquet / CSV readers and writers spanning morloc's Arrow
//! SHM layout. Despite the legacy filename, this module also implements the
//! complementary writers (Arrow IPC, Parquet, CSV) used by the nexus when
//! `--format=arrow|parquet|csv` is selected.

use std::ffi::c_char;
use std::io::Cursor;
use std::ptr;

use arrow_array::cast::AsArray;
use arrow_array::types::*;
use arrow_array::{Array, RecordBatch, StringArray, LargeStringArray};
use arrow_ipc::reader::{FileReader, StreamReader};
use arrow_schema::DataType;

use crate::arrow_ffi::{
    arrow_align_up, arrow_element_size, ArrowColumnDesc, ArrowShmHeader,
    ARROW_SHM_MAGIC, MORLOC_BOOL, MORLOC_FLOAT32, MORLOC_FLOAT64,
    MORLOC_SINT16, MORLOC_SINT32, MORLOC_SINT64, MORLOC_SINT8, MORLOC_STRING,
    MORLOC_UINT16, MORLOC_UINT32, MORLOC_UINT64, MORLOC_UINT8,
};
use crate::cschema::CSchema;
use crate::error::{clear_errmsg, set_errmsg, MorlocError};
use crate::schema::{Schema, SerialType};
use crate::shm::{self, RelPtr};

/// True iff `data` starts with the Arrow IPC file magic. The file format
/// places "ARROW1\0\0" at offset 0; the streaming format does not, so this
/// is a reliable way to distinguish the two.
pub fn is_arrow_file_magic(data: &[u8]) -> bool {
    data.len() >= 8 && &data[..6] == b"ARROW1"
}

/// True iff `data` is a Parquet file: 4-byte magic `PAR1` at both head and
/// tail. The dual-magic check rejects truncated files cheaply.
pub fn is_parquet_magic(data: &[u8]) -> bool {
    data.len() >= 8
        && &data[..4] == b"PAR1"
        && &data[data.len() - 4..] == b"PAR1"
}

/// Map an arrow-rs DataType to the morloc Arrow column-type tag. Returns
/// None for unsupported types (nested arrays, structs, dictionaries, etc.).
fn arrow_dtype_to_morloc(dt: &DataType) -> Option<u32> {
    Some(match dt {
        DataType::Boolean => MORLOC_BOOL,
        DataType::Int8 => MORLOC_SINT8,
        DataType::Int16 => MORLOC_SINT16,
        DataType::Int32 => MORLOC_SINT32,
        DataType::Int64 => MORLOC_SINT64,
        DataType::UInt8 => MORLOC_UINT8,
        DataType::UInt16 => MORLOC_UINT16,
        DataType::UInt32 => MORLOC_UINT32,
        DataType::UInt64 => MORLOC_UINT64,
        DataType::Float32 => MORLOC_FLOAT32,
        DataType::Float64 => MORLOC_FLOAT64,
        DataType::Utf8 | DataType::LargeUtf8 => MORLOC_STRING,
        _ => return None,
    })
}

/// Decide whether a single concatenated batch is compatible with a morloc
/// Map+arrow Schema, returning the per-column expected morloc type. The
/// expected types come from the Schema (the user's typed Table); the actual
/// arrow types must match (modulo widening for variable-width Int -> Sint64
/// and Optional -> nullable).
fn validate_columns(
    rs: &Schema,
    batch: &RecordBatch,
) -> Result<Vec<u32>, MorlocError> {
    let arrow_schema = batch.schema();
    if (arrow_schema.fields().len() != rs.size) || rs.size != rs.keys.len() {
        return Err(MorlocError::Other(format!(
            "Arrow IPC has {} columns, schema expects {}",
            arrow_schema.fields().len(), rs.size,
        )));
    }
    let mut expected = Vec::with_capacity(rs.size);
    for (i, key) in rs.keys.iter().enumerate() {
        let field = arrow_schema.field(i);
        if field.name() != key {
            return Err(MorlocError::Other(format!(
                "Column {} name mismatch: arrow has '{}', schema expects '{}'",
                i, field.name(), key,
            )));
        }
        // Strip a single Optional wrapper from the morloc side. Nullability
        // semantics: nullable arrow into non-Optional morloc is rejected;
        // non-nullable arrow into Optional morloc is permitted (widening).
        let (param_inner, morloc_optional) = match rs.parameters[i].serial_type {
            SerialType::Optional => (
                rs.parameters[i].parameters.first()
                    .map(|c| c.serial_type)
                    .unwrap_or(SerialType::Nil),
                true,
            ),
            other => (other, false),
        };
        // Pragmatic nullability: pyarrow / arrow R / arrow-cpp all mark
        // columns nullable by default even when there are no actual nulls.
        // We accept nullable=true into non-Optional morloc as long as the
        // column's null_count is zero -- this matches the spirit of Stage
        // 0.6 (no information loss, runtime-validated). A non-zero null
        // count is rejected at validation time below in batches_to_shm via
        // the per-column read; here we only block the schema-level
        // mismatch when nulls are actually present.
        if field.is_nullable() && !morloc_optional && batch.column(i).null_count() > 0 {
            return Err(MorlocError::Other(format!(
                "Column '{}' contains {} null(s) but Schema expects non-Optional",
                key, batch.column(i).null_count(),
            )));
        }
        let want = serial_to_arrow(param_inner).ok_or_else(|| MorlocError::Other(format!(
            "Unsupported column type in schema for '{}'", key,
        )))?;
        let got = arrow_dtype_to_morloc(field.data_type()).ok_or_else(|| MorlocError::Other(format!(
            "Unsupported Arrow type for column '{}': {:?}", key, field.data_type(),
        )))?;
        if want != got {
            // Tolerate Int (variable-width) -> Sint64 substitution.
            let same_int_family = (want == MORLOC_SINT64) && (got == MORLOC_SINT64)
                || (param_inner == SerialType::Int && got == MORLOC_SINT64);
            if !same_int_family {
                return Err(MorlocError::Other(format!(
                    "Column '{}' type mismatch: arrow tag 0x{:02x}, schema 0x{:02x}",
                    key, got, want,
                )));
            }
        }
        expected.push(got);
    }
    Ok(expected)
}

/// Same logic as `arrow_ffi::serial_type_to_arrow` but living in this module
/// to keep the upstream private. Variable-width Int / Float collapse to
/// Sint64 / Float64 in the columnar wire format.
fn serial_to_arrow(st: SerialType) -> Option<u32> {
    Some(match st {
        SerialType::Bool => MORLOC_BOOL,
        SerialType::Sint8 => MORLOC_SINT8,
        SerialType::Sint16 => MORLOC_SINT16,
        SerialType::Sint32 => MORLOC_SINT32,
        SerialType::Sint64 | SerialType::Int => MORLOC_SINT64,
        SerialType::Uint8 => MORLOC_UINT8,
        SerialType::Uint16 => MORLOC_UINT16,
        SerialType::Uint32 => MORLOC_UINT32,
        SerialType::Uint64 => MORLOC_UINT64,
        SerialType::Float32 => MORLOC_FLOAT32,
        SerialType::Float64 => MORLOC_FLOAT64,
        SerialType::String => MORLOC_STRING,
        _ => return None,
    })
}

/// Read all record batches from a sequence and concatenate into a single
/// owned RecordBatch (potentially copying), then write out to SHM in
/// morloc's Arrow column layout. Returns RelPtr to the SHM region.
/// Project + cast a sequence of `RecordBatch`es to align with a morloc
/// declared Table schema, applying open-Table semantics.
///
/// Used by readers whose source format (Arrow IPC, Parquet) carries its
/// own typed schema, so unlike CSV / JSON we already know the actual
/// column types the file ships with -- we just have to reconcile them
/// with what the morloc type declares.
///
/// Reconciliation rules (mirroring 'merge_table_schema_with_json' for
/// JSON and 'merge_table_schema_with_csv_header' for CSV):
///
///   * Declared columns are authoritative for *type*. If the morloc
///     type says `x=Int` and the Parquet file's `x` is Int32, the
///     output column has type Int (Sint64) -- arrow-rs's `cast`
///     handles the actual byte-level conversion.
///   * Declared columns are authoritative for *order and presence*.
///     Output order is: declared columns first (in declared order),
///     then file extras in file order.
///   * Missing declared column = error. Same contract as JSON / CSV.
///   * File extras flow through with their actual file types.
///   * If declared cast is not possible (e.g., declared `Int` but the
///     file column is non-numeric), surface the cast failure verbatim.
///
/// Returns the projected batches plus the merged morloc Schema. The
/// caller passes both straight to 'batches_to_shm', which from that
/// point on treats the merged schema as fully-declared -- no further
/// validation happens downstream.
unsafe fn align_batches_to_open_schema(
    batches: Vec<RecordBatch>,
    rs: &Schema,
) -> Result<(Vec<RecordBatch>, Schema), MorlocError> {
    use arrow_array::{ArrayRef, RecordBatch};
    use std::sync::Arc;

    if batches.is_empty() {
        // Empty input: produce an empty merged schema. If declared
        // columns exist they are dropped (no rows to validate against);
        // a downstream consumer of an empty Table is by definition
        // schema-agnostic.
        return Ok((batches, rs.clone()));
    }

    // The file-side schema is shared across all batches in IPC /
    // Parquet readers (they reject schema-shifting mid-stream).
    let file_arrow_schema = batches[0].schema();

    // Index file columns by name for O(1) "where is column k" lookup.
    let mut file_idx: std::collections::HashMap<String, usize> = std::collections::HashMap::new();
    for (i, f) in file_arrow_schema.fields().iter().enumerate() {
        file_idx.insert(f.name().clone(), i);
    }

    // Validate declared columns are present in the file.
    for k in &rs.keys {
        if !file_idx.contains_key(k) {
            return Err(MorlocError::Other(format!(
                "Declared column '{}' missing from input file", k
            )));
        }
    }

    // Build merged morloc Schema: declared first, then file extras
    // not in declared.
    let mut merged_keys: Vec<String> = Vec::with_capacity(file_arrow_schema.fields().len());
    let mut merged_params: Vec<Schema> = Vec::with_capacity(merged_keys.capacity());
    let mut declared_set: std::collections::HashSet<&str> = std::collections::HashSet::new();
    for k in &rs.keys { declared_set.insert(k.as_str()); }

    for (i, k) in rs.keys.iter().enumerate() {
        merged_keys.push(k.clone());
        merged_params.push(rs.parameters[i].clone());
    }
    for f in file_arrow_schema.fields() {
        if declared_set.contains(f.name().as_str()) { continue; }
        let inner = Schema::primitive(arrow_dtype_to_serial_type(f.data_type()));
        merged_keys.push(f.name().clone());
        // Tables that arrive from typed file formats inherit the
        // file's nullability (true if the Arrow field is nullable).
        merged_params.push(maybe_optional_csv(inner, f.is_nullable()));
    }
    let merged_rs = Schema {
        serial_type: SerialType::Table,
        size: merged_keys.len(),
        width: std::mem::size_of::<crate::shm::Array>(),
        offsets: Vec::new(),
        hint: None,
        parameters: merged_params,
        keys: merged_keys.clone(),
        name: None,
    };

    // Build the per-batch projected+cast columns. Order matches
    // merged_rs.keys; types match merged_rs.parameters (which for
    // declared columns is the morloc-declared type, requiring a cast
    // when the file type differs).
    let mut projected: Vec<RecordBatch> = Vec::with_capacity(batches.len());
    for batch in &batches {
        let mut new_cols: Vec<ArrayRef> = Vec::with_capacity(merged_rs.size);
        let mut new_fields: Vec<arrow_schema::Field> = Vec::with_capacity(merged_rs.size);
        for (i, k) in merged_rs.keys.iter().enumerate() {
            let src_idx = *file_idx.get(k).expect("merged key must be in file");
            let src_col = batch.column(src_idx);
            let target = morloc_param_to_arrow_dtype(&merged_rs.parameters[i])?;
            let target_dt = target.0;
            let nullable = target.1;
            let cast_col = if src_col.data_type() == &target_dt {
                src_col.clone()
            } else {
                arrow_cast::cast(src_col, &target_dt).map_err(|e| MorlocError::Other(format!(
                    "Cannot cast column '{}' from {:?} to {:?}: {}",
                    k, src_col.data_type(), target_dt, e
                )))?
            };
            new_cols.push(cast_col);
            new_fields.push(arrow_schema::Field::new(k.clone(), target_dt, nullable));
        }
        let new_schema = Arc::new(arrow_schema::Schema::new(new_fields));
        let new_batch = RecordBatch::try_new(new_schema, new_cols).map_err(|e| MorlocError::Other(format!(
            "Failed to project record batch: {}", e
        )))?;
        projected.push(new_batch);
    }

    Ok((projected, merged_rs))
}

/// Map a morloc per-column 'Schema' (possibly Optional-wrapped) to the
/// matching arrow-rs (DataType, nullable) pair. Used by
/// 'align_batches_to_open_schema' to know what type to cast each
/// declared column to.
fn morloc_param_to_arrow_dtype(p: &Schema) -> Result<(arrow_schema::DataType, bool), MorlocError> {
    use arrow_schema::DataType as DT;
    let (inner, nullable) = if p.serial_type == SerialType::Optional {
        (p.parameters.first()
            .map(|c| c.serial_type)
            .unwrap_or(SerialType::Nil), true)
    } else {
        (p.serial_type, false)
    };
    let dt = match inner {
        SerialType::Bool => DT::Boolean,
        SerialType::Sint8 => DT::Int8,
        SerialType::Sint16 => DT::Int16,
        SerialType::Sint32 => DT::Int32,
        SerialType::Sint64 | SerialType::Int => DT::Int64,
        SerialType::Uint8 => DT::UInt8,
        SerialType::Uint16 => DT::UInt16,
        SerialType::Uint32 => DT::UInt32,
        SerialType::Uint64 => DT::UInt64,
        SerialType::Float32 => DT::Float32,
        SerialType::Float64 => DT::Float64,
        SerialType::String => DT::Utf8,
        other => return Err(MorlocError::Other(format!(
            "Unsupported morloc Table column type: {:?}", other,
        ))),
    };
    Ok((dt, nullable))
}

unsafe fn batches_to_shm(
    batches: Vec<RecordBatch>,
    rs: &Schema,
    errmsg: *mut *mut c_char,
) -> RelPtr {
    if batches.is_empty() {
        // Permitted: schema-only file -> Table 0 r.
        return write_empty_arrow_shm(rs, errmsg);
    }

    let batch = if batches.len() == 1 {
        batches.into_iter().next().unwrap()
    } else {
        match arrow_select::concat::concat_batches(&batches[0].schema(), &batches) {
            Ok(b) => b,
            Err(e) => {
                set_errmsg(errmsg, &MorlocError::Other(
                    format!("Failed to concat Arrow record batches: {}", e),
                ));
                return shm::RELNULL;
            }
        }
    };

    let col_types = match validate_columns(rs, &batch) {
        Ok(v) => v,
        Err(e) => { set_errmsg(errmsg, &e); return shm::RELNULL; }
    };

    let n_cols = batch.num_columns();
    let n_rows = batch.num_rows();

    // Compute SHM layout. Same recipe as the JSON path.
    let header_size = std::mem::size_of::<ArrowShmHeader>();
    let descs_size = n_cols * std::mem::size_of::<ArrowColumnDesc>();
    let names_size: usize = rs.keys.iter().map(|k| k.len()).sum();
    let data_start = arrow_align_up(header_size + descs_size + names_size);

    // Pre-compute string-column data sizes.
    let mut str_sizes = vec![0usize; n_cols];
    for c in 0..n_cols {
        if col_types[c] == MORLOC_STRING {
            let arr = batch.column(c);
            str_sizes[c] = if let Some(sa) = arr.as_any().downcast_ref::<StringArray>() {
                sa.value_data().len()
            } else if let Some(la) = arr.as_any().downcast_ref::<LargeStringArray>() {
                la.value_data().len()
            } else {
                0
            };
        }
    }

    let mut total_size = data_start;
    for c in 0..n_cols {
        total_size = arrow_align_up(total_size);
        if col_types[c] == MORLOC_STRING {
            total_size += (n_rows + 1) * std::mem::size_of::<i32>() + str_sizes[c];
        } else {
            total_size += arrow_element_size(col_types[c]) * n_rows;
        }
    }

    let shm_ptr = match shm::shmalloc(total_size) {
        Ok(p) => p,
        Err(e) => { set_errmsg(errmsg, &e); return shm::RELNULL; }
    };
    ptr::write_bytes(shm_ptr, 0, total_size);

    let header = &mut *(shm_ptr as *mut ArrowShmHeader);
    header.magic = ARROW_SHM_MAGIC;
    header.n_columns = n_cols as u32;
    header.n_rows = n_rows as u64;
    header.total_size = total_size as u64;

    let descs = shm_ptr.add(header_size) as *mut ArrowColumnDesc;
    let mut name_cursor = header_size + descs_size;
    let mut data_cursor = data_start;

    for c in 0..n_cols {
        data_cursor = arrow_align_up(data_cursor);
        let key = &rs.keys[c];
        let key_len = key.len();
        let desc = &mut *descs.add(c);
        desc.col_type = col_types[c];
        desc.length = n_rows as u64;
        desc.null_count = batch.column(c).null_count() as u64;
        desc.name_offset = name_cursor as u32;
        desc.name_length = key_len as u16;
        desc.data_offset = data_cursor as u64;

        if key_len > 0 {
            ptr::copy_nonoverlapping(key.as_ptr(), shm_ptr.add(name_cursor), key_len);
        }
        name_cursor += key_len;

        let buf_size = match write_column_from_arrow(
            shm_ptr.add(data_cursor),
            col_types[c],
            batch.column(c).as_ref(),
            n_rows,
            errmsg,
        ) {
            Some(sz) => sz,
            None => { let _ = shm::shfree(shm_ptr); return shm::RELNULL; }
        };
        desc.data_size = buf_size as u64;
        data_cursor += buf_size;
    }

    match shm::abs2rel(shm_ptr) {
        Ok(r) => r,
        Err(e) => {
            let _ = shm::shfree(shm_ptr);
            set_errmsg(errmsg, &e);
            shm::RELNULL
        }
    }
}

/// Write an empty (zero-row) ArrowShm. Used when the IPC file contains a
/// schema but no record batches.
unsafe fn write_empty_arrow_shm(rs: &Schema, errmsg: *mut *mut c_char) -> RelPtr {
    let n_cols = rs.size;
    let header_size = std::mem::size_of::<ArrowShmHeader>();
    let descs_size = n_cols * std::mem::size_of::<ArrowColumnDesc>();
    let names_size: usize = rs.keys.iter().map(|k| k.len()).sum();
    let data_start = arrow_align_up(header_size + descs_size + names_size);
    let total_size = data_start + n_cols * std::mem::size_of::<i32>(); // tiny pad for offsets

    let shm_ptr = match shm::shmalloc(total_size) {
        Ok(p) => p,
        Err(e) => { set_errmsg(errmsg, &e); return shm::RELNULL; }
    };
    ptr::write_bytes(shm_ptr, 0, total_size);

    let header = &mut *(shm_ptr as *mut ArrowShmHeader);
    header.magic = ARROW_SHM_MAGIC;
    header.n_columns = n_cols as u32;
    header.n_rows = 0;
    header.total_size = total_size as u64;

    let descs = shm_ptr.add(header_size) as *mut ArrowColumnDesc;
    let mut name_cursor = header_size + descs_size;
    let mut data_cursor = data_start;
    for c in 0..n_cols {
        data_cursor = arrow_align_up(data_cursor);
        let key = &rs.keys[c];
        let key_len = key.len();
        let want_arrow = serial_to_arrow(match rs.parameters[c].serial_type {
            SerialType::Optional => rs.parameters[c].parameters.first()
                .map(|c| c.serial_type)
                .unwrap_or(SerialType::Nil),
            other => other,
        }).unwrap_or(MORLOC_SINT64);
        let desc = &mut *descs.add(c);
        desc.col_type = want_arrow;
        desc.length = 0;
        desc.null_count = 0;
        desc.name_offset = name_cursor as u32;
        desc.name_length = key_len as u16;
        desc.data_offset = data_cursor as u64;
        desc.data_size = 0;
        if key_len > 0 {
            ptr::copy_nonoverlapping(key.as_ptr(), shm_ptr.add(name_cursor), key_len);
        }
        name_cursor += key_len;
    }
    match shm::abs2rel(shm_ptr) {
        Ok(r) => r,
        Err(e) => {
            let _ = shm::shfree(shm_ptr);
            set_errmsg(errmsg, &e);
            shm::RELNULL
        }
    }
}

/// Copy a single column's data from the arrow-rs Array into the SHM column
/// buffer at `dst`. Returns bytes written.
unsafe fn write_column_from_arrow(
    dst: *mut u8,
    col_type: u32,
    arr: &dyn Array,
    n_rows: usize,
    errmsg: *mut *mut c_char,
) -> Option<usize> {
    macro_rules! copy_primitive {
        ($t:ty, $arrow_t:ty) => {{
            let typed = arr.as_primitive::<$arrow_t>();
            let values = typed.values();
            let elem = std::mem::size_of::<$t>();
            let bytes = elem * n_rows;
            if bytes > 0 {
                ptr::copy_nonoverlapping(values.as_ptr() as *const u8, dst, bytes);
            }
            Some(bytes)
        }};
    }

    match col_type {
        MORLOC_BOOL => {
            let typed = arr.as_boolean();
            for i in 0..n_rows {
                *dst.add(i) = if typed.value(i) { 1 } else { 0 };
            }
            Some(n_rows)
        }
        MORLOC_SINT8  => copy_primitive!(i8, Int8Type),
        MORLOC_SINT16 => copy_primitive!(i16, Int16Type),
        MORLOC_SINT32 => copy_primitive!(i32, Int32Type),
        MORLOC_SINT64 => {
            // Could be either Int64 or the variable-width morloc Int.
            // After validate_columns the arrow side is Int64.
            copy_primitive!(i64, Int64Type)
        }
        MORLOC_UINT8  => copy_primitive!(u8, UInt8Type),
        MORLOC_UINT16 => copy_primitive!(u16, UInt16Type),
        MORLOC_UINT32 => copy_primitive!(u32, UInt32Type),
        MORLOC_UINT64 => copy_primitive!(u64, UInt64Type),
        MORLOC_FLOAT32 => copy_primitive!(f32, Float32Type),
        MORLOC_FLOAT64 => copy_primitive!(f64, Float64Type),
        MORLOC_STRING => {
            // Layout in SHM: (n+1) i32 offsets, then bytes. Both StringArray
            // (i32 offsets) and LargeStringArray (i64 offsets) compress to
            // the i32 offset form here -- if a LargeString row exceeds 2GB
            // we error out rather than silently truncate.
            let offsets_dst = dst as *mut i32;
            *offsets_dst.add(0) = 0;
            if let Some(sa) = arr.as_any().downcast_ref::<StringArray>() {
                let value_data = sa.value_data();
                let value_offsets = sa.value_offsets();
                let base = value_offsets[0];
                let total = (value_offsets[n_rows] - base) as usize;
                for r in 0..n_rows {
                    *offsets_dst.add(r + 1) = (value_offsets[r + 1] - base) as i32;
                }
                let str_dst = (offsets_dst.add(n_rows + 1)) as *mut u8;
                if total > 0 {
                    ptr::copy_nonoverlapping(
                        value_data[(base as usize)..].as_ptr(),
                        str_dst,
                        total,
                    );
                }
                Some((n_rows + 1) * std::mem::size_of::<i32>() + total)
            } else if let Some(la) = arr.as_any().downcast_ref::<LargeStringArray>() {
                let value_data = la.value_data();
                let value_offsets = la.value_offsets();
                let base = value_offsets[0];
                let total = (value_offsets[n_rows] - base) as usize;
                if total > i32::MAX as usize {
                    set_errmsg(errmsg, &MorlocError::Other(
                        "LargeUtf8 column exceeds 2 GiB; not supported in morloc Arrow SHM".into(),
                    ));
                    return None;
                }
                for r in 0..n_rows {
                    let off = (value_offsets[r + 1] - base) as i64;
                    if off > i32::MAX as i64 {
                        set_errmsg(errmsg, &MorlocError::Other(
                            "LargeUtf8 offset exceeds 2 GiB".into(),
                        ));
                        return None;
                    }
                    *offsets_dst.add(r + 1) = off as i32;
                }
                let str_dst = (offsets_dst.add(n_rows + 1)) as *mut u8;
                if total > 0 {
                    ptr::copy_nonoverlapping(
                        value_data[(base as usize)..].as_ptr(),
                        str_dst,
                        total,
                    );
                }
                Some((n_rows + 1) * std::mem::size_of::<i32>() + total)
            } else {
                set_errmsg(errmsg, &MorlocError::Other(
                    "String column has unexpected layout".into(),
                ));
                None
            }
        }
        _ => {
            set_errmsg(errmsg, &MorlocError::Other(
                format!("Unsupported Arrow column type 0x{:02x}", col_type),
            ));
            None
        }
    }
}

/// Reverse of the column writer: read an Arrow SHM region into an
/// arrow-rs RecordBatch by copying out the columns. Used by the file
/// writers (IPC / Parquet / CSV).
unsafe fn arrow_shm_to_record_batch(
    header: *const ArrowShmHeader,
) -> Result<RecordBatch, MorlocError> {
    use arrow_array::*;
    use arrow_schema::{DataType, Field};
    use std::sync::Arc;

    if header.is_null() {
        return Err(MorlocError::Other("NULL Arrow SHM header".into()));
    }
    if (*header).magic != ARROW_SHM_MAGIC {
        return Err(MorlocError::Other("Invalid Arrow SHM magic".into()));
    }
    let n_cols = (*header).n_columns as usize;
    let n_rows = (*header).n_rows as usize;

    let mut fields = Vec::with_capacity(n_cols);
    let mut columns: Vec<ArrayRef> = Vec::with_capacity(n_cols);

    for c in 0..n_cols {
        let desc = crate::arrow_ffi::arrow_column_desc(header, c as u32);
        if desc.is_null() {
            return Err(MorlocError::Other(format!("NULL column desc at {}", c)));
        }
        let raw_name = crate::arrow_ffi::arrow_column_name(header, c as u32);
        let name_len = (*desc).name_length as usize;
        let name = if !raw_name.is_null() && name_len > 0 {
            let slice = std::slice::from_raw_parts(raw_name as *const u8, name_len);
            std::str::from_utf8(slice).unwrap_or("").to_string()
        } else {
            String::new()
        };
        let col_type = (*desc).col_type;
        let buf = crate::arrow_ffi::arrow_column_data(header, c as u32) as *const u8;

        let (dt, arr): (DataType, ArrayRef) = match col_type {
            x if x == MORLOC_BOOL => {
                let mut bs: Vec<bool> = Vec::with_capacity(n_rows);
                for i in 0..n_rows { bs.push(*buf.add(i) != 0); }
                (DataType::Boolean, Arc::new(BooleanArray::from(bs)))
            }
            x if x == MORLOC_SINT8 => {
                let s = std::slice::from_raw_parts(buf as *const i8, n_rows).to_vec();
                (DataType::Int8, Arc::new(Int8Array::from(s)))
            }
            x if x == MORLOC_SINT16 => {
                let s = std::slice::from_raw_parts(buf as *const i16, n_rows).to_vec();
                (DataType::Int16, Arc::new(Int16Array::from(s)))
            }
            x if x == MORLOC_SINT32 => {
                let s = std::slice::from_raw_parts(buf as *const i32, n_rows).to_vec();
                (DataType::Int32, Arc::new(Int32Array::from(s)))
            }
            x if x == MORLOC_SINT64 => {
                let s = std::slice::from_raw_parts(buf as *const i64, n_rows).to_vec();
                (DataType::Int64, Arc::new(Int64Array::from(s)))
            }
            x if x == MORLOC_UINT8 => {
                let s = std::slice::from_raw_parts(buf, n_rows).to_vec();
                (DataType::UInt8, Arc::new(UInt8Array::from(s)))
            }
            x if x == MORLOC_UINT16 => {
                let s = std::slice::from_raw_parts(buf as *const u16, n_rows).to_vec();
                (DataType::UInt16, Arc::new(UInt16Array::from(s)))
            }
            x if x == MORLOC_UINT32 => {
                let s = std::slice::from_raw_parts(buf as *const u32, n_rows).to_vec();
                (DataType::UInt32, Arc::new(UInt32Array::from(s)))
            }
            x if x == MORLOC_UINT64 => {
                let s = std::slice::from_raw_parts(buf as *const u64, n_rows).to_vec();
                (DataType::UInt64, Arc::new(UInt64Array::from(s)))
            }
            x if x == MORLOC_FLOAT32 => {
                let s = std::slice::from_raw_parts(buf as *const f32, n_rows).to_vec();
                (DataType::Float32, Arc::new(Float32Array::from(s)))
            }
            x if x == MORLOC_FLOAT64 => {
                let s = std::slice::from_raw_parts(buf as *const f64, n_rows).to_vec();
                (DataType::Float64, Arc::new(Float64Array::from(s)))
            }
            x if x == MORLOC_STRING => {
                let offsets = buf as *const i32;
                let str_data = (offsets.add(n_rows + 1)) as *const u8;
                let mut strings: Vec<&str> = Vec::with_capacity(n_rows);
                let mut owned: Vec<String> = Vec::with_capacity(n_rows);
                for r in 0..n_rows {
                    let start = *offsets.add(r) as usize;
                    let end = *offsets.add(r + 1) as usize;
                    let bytes = std::slice::from_raw_parts(str_data.add(start), end - start);
                    owned.push(std::str::from_utf8(bytes).unwrap_or("").to_string());
                }
                for s in &owned { strings.push(s.as_str()); }
                let arr: ArrayRef = Arc::new(StringArray::from(strings));
                (DataType::Utf8, arr)
            }
            other => return Err(MorlocError::Other(format!(
                "Unsupported Arrow column type 0x{:02x}", other,
            ))),
        };
        fields.push(Field::new(&name, dt, false));
        columns.push(arr);
    }

    let schema = Arc::new(arrow_schema::Schema::new(fields));
    RecordBatch::try_new(schema, columns).map_err(|e| MorlocError::Other(format!(
        "Failed to build RecordBatch from SHM: {}", e,
    )))
}

/// Serialize an Arrow SHM-resident table to Arrow IPC file bytes.
/// Returned buffer is allocated with libc::malloc; caller must free via
/// `libc::free`. On error returns null and sets `errmsg`.
///
/// # Safety
/// `header` must be a valid Arrow SHM header pointer.
#[no_mangle]
pub unsafe extern "C" fn write_arrow_ipc_to_buffer(
    header: *const ArrowShmHeader,
    out_buf: *mut *mut u8,
    out_len: *mut usize,
    errmsg: *mut *mut c_char,
) -> i32 {
    use arrow_ipc::writer::FileWriter;
    clear_errmsg(errmsg);
    *out_buf = ptr::null_mut();
    *out_len = 0;

    let batch = match arrow_shm_to_record_batch(header) {
        Ok(b) => b,
        Err(e) => { set_errmsg(errmsg, &e); return 1; }
    };
    let mut buf: Vec<u8> = Vec::new();
    {
        let mut writer = match FileWriter::try_new(&mut buf, &batch.schema()) {
            Ok(w) => w,
            Err(e) => {
                set_errmsg(errmsg, &MorlocError::Other(
                    format!("FileWriter::try_new: {}", e),
                ));
                return 1;
            }
        };
        if let Err(e) = writer.write(&batch) {
            set_errmsg(errmsg, &MorlocError::Other(
                format!("Arrow IPC write: {}", e),
            ));
            return 1;
        }
        if let Err(e) = writer.finish() {
            set_errmsg(errmsg, &MorlocError::Other(
                format!("Arrow IPC finish: {}", e),
            ));
            return 1;
        }
    }
    let len = buf.len();
    let mem = libc::malloc(len) as *mut u8;
    if mem.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("malloc failed".into()));
        return 1;
    }
    ptr::copy_nonoverlapping(buf.as_ptr(), mem, len);
    *out_buf = mem;
    *out_len = len;
    0
}

/// Serialize an Arrow SHM-resident table to Parquet bytes. See
/// `write_arrow_ipc_to_buffer` for the buffer ownership convention.
#[no_mangle]
pub unsafe extern "C" fn write_parquet_to_buffer(
    header: *const ArrowShmHeader,
    out_buf: *mut *mut u8,
    out_len: *mut usize,
    errmsg: *mut *mut c_char,
) -> i32 {
    use parquet::arrow::ArrowWriter;
    clear_errmsg(errmsg);
    *out_buf = ptr::null_mut();
    *out_len = 0;

    let batch = match arrow_shm_to_record_batch(header) {
        Ok(b) => b,
        Err(e) => { set_errmsg(errmsg, &e); return 1; }
    };
    let mut buf: Vec<u8> = Vec::new();
    {
        let mut writer = match ArrowWriter::try_new(&mut buf, batch.schema(), None) {
            Ok(w) => w,
            Err(e) => {
                set_errmsg(errmsg, &MorlocError::Other(
                    format!("Parquet writer init: {}", e),
                ));
                return 1;
            }
        };
        if let Err(e) = writer.write(&batch) {
            set_errmsg(errmsg, &MorlocError::Other(format!("Parquet write: {}", e)));
            return 1;
        }
        if let Err(e) = writer.close() {
            set_errmsg(errmsg, &MorlocError::Other(format!("Parquet close: {}", e)));
            return 1;
        }
    }
    let len = buf.len();
    let mem = libc::malloc(len) as *mut u8;
    if mem.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("malloc failed".into()));
        return 1;
    }
    ptr::copy_nonoverlapping(buf.as_ptr(), mem, len);
    *out_buf = mem;
    *out_len = len;
    0
}

/// Serialize an Arrow SHM-resident table to CSV bytes (with header row).
/// The `delimiter` byte selects ',' for CSV and '\t' for TSV. See
/// `write_arrow_ipc_to_buffer` for the buffer ownership convention.
#[no_mangle]
pub unsafe extern "C" fn write_csv_to_buffer(
    header: *const ArrowShmHeader,
    delimiter: u8,
    out_buf: *mut *mut u8,
    out_len: *mut usize,
    errmsg: *mut *mut c_char,
) -> i32 {
    use arrow_csv::writer::WriterBuilder;
    clear_errmsg(errmsg);
    *out_buf = ptr::null_mut();
    *out_len = 0;

    let batch = match arrow_shm_to_record_batch(header) {
        Ok(b) => b,
        Err(e) => { set_errmsg(errmsg, &e); return 1; }
    };
    let mut buf: Vec<u8> = Vec::new();
    {
        let mut writer = WriterBuilder::new()
            .with_header(true)
            .with_delimiter(delimiter)
            .build(&mut buf);
        if let Err(e) = writer.write(&batch) {
            set_errmsg(errmsg, &MorlocError::Other(format!("CSV write: {}", e)));
            return 1;
        }
    }
    let len = buf.len();
    let mem = libc::malloc(len) as *mut u8;
    if mem.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("malloc failed".into()));
        return 1;
    }
    ptr::copy_nonoverlapping(buf.as_ptr(), mem, len);
    *out_buf = mem;
    *out_len = len;
    0
}

/// Read an Arrow IPC file (ARROW1 magic) or stream, validate against the
/// morloc Schema, and land the result in SHM.
///
/// # Safety
/// `data` must point to `data_len` valid bytes. `schema` must be a valid
/// CSchema pointer or null.
#[no_mangle]
pub unsafe extern "C" fn read_arrow_ipc_to_shm(
    data: *const u8,
    data_len: usize,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> RelPtr {
    clear_errmsg(errmsg);
    if data.is_null() || schema.is_null() || data_len == 0 {
        set_errmsg(errmsg, &MorlocError::Other("NULL data or schema".into()));
        return shm::RELNULL;
    }
    let bytes = std::slice::from_raw_parts(data, data_len);
    let rs = CSchema::to_rust(schema);
    if !crate::arrow_ffi::is_arrow_table_schema(&rs) {
        set_errmsg(errmsg, &MorlocError::Other(
            "Arrow IPC reader requires Map+arrow schema".into(),
        ));
        return shm::RELNULL;
    }

    // Try file format first (when we have the magic), else fall back to
    // stream format. The file reader is more efficient because it can
    // parallelise / skip via the footer.
    let batches = if is_arrow_file_magic(bytes) {
        let cursor = Cursor::new(bytes);
        let reader = match FileReader::try_new(cursor, None) {
            Ok(r) => r,
            Err(e) => {
                set_errmsg(errmsg, &MorlocError::Other(
                    format!("Failed to open Arrow IPC file: {}", e),
                ));
                return shm::RELNULL;
            }
        };
        match reader.collect::<Result<Vec<_>, _>>() {
            Ok(v) => v,
            Err(e) => {
                set_errmsg(errmsg, &MorlocError::Other(
                    format!("Failed to read Arrow IPC batches: {}", e),
                ));
                return shm::RELNULL;
            }
        }
    } else {
        let cursor = Cursor::new(bytes);
        let reader = match StreamReader::try_new(cursor, None) {
            Ok(r) => r,
            Err(e) => {
                set_errmsg(errmsg, &MorlocError::Other(
                    format!("Failed to open Arrow IPC stream: {}", e),
                ));
                return shm::RELNULL;
            }
        };
        match reader.collect::<Result<Vec<_>, _>>() {
            Ok(v) => v,
            Err(e) => {
                set_errmsg(errmsg, &MorlocError::Other(
                    format!("Failed to read Arrow IPC stream batches: {}", e),
                ));
                return shm::RELNULL;
            }
        }
    };

    // Open Table semantics: align the file's columns with the morloc
    // declared schema. Declared columns are coerced to the declared
    // types (cast where the file's type differs); file-only columns
    // flow through with their actual types appended after the
    // declared set.
    let (aligned, merged_rs) = match align_batches_to_open_schema(batches, &rs) {
        Ok(p) => p,
        Err(e) => { set_errmsg(errmsg, &e); return shm::RELNULL; }
    };

    batches_to_shm(aligned, &merged_rs, errmsg)
}

/// Build an arrow-rs Schema from a morloc Map+arrow Schema. Used by the
/// CSV reader (which needs a typed schema to drive parsing) and any other
/// path that wants to delegate to arrow-rs's typed APIs.
fn morloc_to_arrow_schema(rs: &Schema) -> Result<arrow_schema::Schema, MorlocError> {
    use arrow_schema::{DataType, Field};
    let mut fields = Vec::with_capacity(rs.size);
    for (i, key) in rs.keys.iter().enumerate() {
        let p = &rs.parameters[i];
        let (inner, nullable) = if p.serial_type == SerialType::Optional {
            (p.parameters.first()
                .map(|c| c.serial_type)
                .unwrap_or(SerialType::Nil), true)
        } else { (p.serial_type, false) };
        let dt = match inner {
            SerialType::Bool => DataType::Boolean,
            SerialType::Sint8 => DataType::Int8,
            SerialType::Sint16 => DataType::Int16,
            SerialType::Sint32 => DataType::Int32,
            SerialType::Sint64 | SerialType::Int => DataType::Int64,
            SerialType::Uint8 => DataType::UInt8,
            SerialType::Uint16 => DataType::UInt16,
            SerialType::Uint32 => DataType::UInt32,
            SerialType::Uint64 => DataType::UInt64,
            SerialType::Float32 => DataType::Float32,
            SerialType::Float64 => DataType::Float64,
            SerialType::String => DataType::Utf8,
            other => return Err(MorlocError::Other(format!(
                "Unsupported column type for '{}': {:?}", key, other,
            ))),
        };
        fields.push(Field::new(key, dt, nullable));
    }
    Ok(arrow_schema::Schema::new(fields))
}

/// Map an arrow-rs 'DataType' (the result of CSV / Parquet schema
/// inference) to the matching morloc 'SerialType'. Used by the CSV
/// reader to surface inferred column types for un-declared columns
/// in the open-semantics merge. Best-effort: variant types arrow-rs
/// can produce that morloc has no analogue for (e.g., dates, decimals)
/// fall through to 'SerialType::String', which round-trips the raw
/// CSV cell text without lossy coercion.
fn arrow_dtype_to_serial_type(dt: &arrow_schema::DataType) -> SerialType {
    use arrow_schema::DataType as DT;
    match dt {
        DT::Boolean => SerialType::Bool,
        DT::Int8 => SerialType::Sint8,
        DT::Int16 => SerialType::Sint16,
        DT::Int32 => SerialType::Sint32,
        DT::Int64 => SerialType::Sint64,
        DT::UInt8 => SerialType::Uint8,
        DT::UInt16 => SerialType::Uint16,
        DT::UInt32 => SerialType::Uint32,
        DT::UInt64 => SerialType::Uint64,
        DT::Float32 => SerialType::Float32,
        DT::Float64 => SerialType::Float64,
        DT::Utf8 | DT::LargeUtf8 => SerialType::String,
        _ => SerialType::String,
    }
}

/// Wrap a primitive 'Schema' in 'SerialType::Optional' when @nullable@.
/// Mirrors 'arrow_ffi::maybe_optional' but is duplicated here to keep
/// 'arrow_ipc_reader' self-contained (the FFI module is the canonical
/// home for the JSON-side helpers; CSV merge logic lives here).
fn maybe_optional_csv(inner: Schema, nullable: bool) -> Schema {
    if !nullable { return inner; }
    // Optional's voidstar slot is a relptr (RELNULL = absent); width
    // and alignment are pointer-sized. Mirrors schema.rs's
    // `make_optional_schema`.
    Schema {
        serial_type: SerialType::Optional,
        size: 1,
        width: std::mem::size_of::<crate::shm::RelPtr>(),
        offsets: Vec::new(),
        hint: None,
        parameters: vec![inner],
        keys: Vec::new(),
        name: None,
    }
}

/// Merge a declared morloc Table schema with a CSV-inferred arrow_schema.
///
/// Implements the same *open* Table semantics as the JSON path:
/// declared columns are authoritative for their types; columns the
/// CSV has but the morloc declaration does not are appended with
/// arrow-rs-inferred types.
///
/// Order: declared columns first (in declared order), then any extras
/// in CSV-header order.
///
/// Returns a Vec of (column-name, morloc-declared-or-inferred Schema)
/// pairs that the caller turns into both:
///   * an arrow_schema::Schema to drive the CSV ReaderBuilder, and
///   * a morloc 'Schema' for 'batches_to_shm' to lay out SHM columns.
fn merge_table_schema_with_csv_header(
    rs: &Schema,
    inferred: &arrow_schema::Schema,
) -> Result<Schema, MorlocError> {
    let mut declared_idx: std::collections::HashMap<&str, usize> = std::collections::HashMap::new();
    for (i, k) in rs.keys.iter().enumerate() {
        declared_idx.insert(k.as_str(), i);
    }

    // Validate declared columns are present in the CSV header.
    for k in &rs.keys {
        let mut found = false;
        for f in inferred.fields() {
            if f.name() == k { found = true; break; }
        }
        if !found {
            return Err(MorlocError::Other(format!(
                "Declared column '{}' missing from CSV header", k
            )));
        }
    }

    let mut keys: Vec<String> = Vec::with_capacity(rs.keys.len() + inferred.fields().len());
    let mut params: Vec<Schema> = Vec::with_capacity(keys.capacity());

    // 1. Declared columns first.
    for (i, k) in rs.keys.iter().enumerate() {
        keys.push(k.clone());
        params.push(rs.parameters[i].clone());
    }

    // 2. CSV-header columns not in the declared set, in CSV-header order.
    for f in inferred.fields() {
        let name = f.name();
        if declared_idx.contains_key(name.as_str()) { continue; }
        let inner = Schema::primitive(arrow_dtype_to_serial_type(f.data_type()));
        keys.push(name.clone());
        params.push(maybe_optional_csv(inner, f.is_nullable()));
    }

    Ok(Schema {
        serial_type: SerialType::Table,
        size: keys.len(),
        width: std::mem::size_of::<crate::shm::Array>(),
        offsets: Vec::new(),
        hint: None,
        parameters: params,
        keys,
        name: None,
    })
}

/// Resolve the sniff-window size for CSV type inference.
///
/// Default is 100 records, which is enough to catch the typical
/// "first row was an integer-looking string but later rows are
/// floats" pattern that arises in real-world CSV (a column with
/// values `1, 2, 3, 4.5` would mis-infer to `Int64` if we only
/// sniffed the first row).
///
/// Override with the `MORLOC_CSV_SNIFF_ROWS` environment variable:
///   * a positive integer -> sniff at most that many records before
///     finalising types,
///   * `0` -> sniff zero data rows, falling back to `String` for
///     every column. Use this when your data has rare value types
///     deep in the file that would mislead a prefix sniff.
///
/// Returned as `Option<usize>` because that is what
/// `arrow_csv::Format::infer_schema` accepts: `None` means "scan the
/// entire input", which we never want here (huge files would blow
/// memory). Any non-zero override caps the scan; 0 disables.
fn csv_sniff_rows() -> Option<usize> {
    const DEFAULT: usize = 100;
    match std::env::var("MORLOC_CSV_SNIFF_ROWS") {
        Ok(s) => match s.parse::<usize>() {
            Ok(n) => Some(n),
            // A non-numeric override silently falls back to the default
            // rather than failing the whole load -- the user gets
            // sensible behaviour while learning the variable's syntax.
            Err(_) => Some(DEFAULT),
        },
        Err(_) => Some(DEFAULT),
    }
}

/// Read a CSV / TSV file into the morloc Arrow SHM layout.
///
/// Open Table semantics: the morloc target type is a constraint on
/// (and parser hint for) the column set, but never an upper bound:
///
///   * Bare `T` (no declared columns) -- arrow-rs's CSV inference
///     determines all column names and types from the header + samples.
///   * `T:K<entries>` (declared columns) -- declared columns are
///     coerced to the declared morloc types regardless of what the
///     CSV looks like (within Arrow's casting rules); other columns
///     in the CSV header are appended with inferred types.
///
/// Concretely we always run arrow-csv's `Format::infer_schema` over
/// the bytes first, then merge with the declared morloc schema in
/// 'merge_table_schema_with_csv_header'. The merged schema drives the
/// final `ReaderBuilder`, so the parser knows the exact type to use
/// per column without sniffing twice.
///
/// # Safety
/// `data` must point to `data_len` valid bytes. `schema` must be a valid
/// CSchema pointer or null.
#[no_mangle]
pub unsafe extern "C" fn read_csv_to_shm(
    data: *const u8,
    data_len: usize,
    delimiter: u8,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> RelPtr {
    use arrow_csv::ReaderBuilder;

    clear_errmsg(errmsg);
    if data.is_null() || schema.is_null() || data_len == 0 {
        set_errmsg(errmsg, &MorlocError::Other("NULL data or schema".into()));
        return shm::RELNULL;
    }
    let bytes = std::slice::from_raw_parts(data, data_len);
    let rs = CSchema::to_rust(schema);
    if !crate::arrow_ffi::is_arrow_table_schema(&rs) {
        set_errmsg(errmsg, &MorlocError::Other(
            "CSV reader requires a morloc Table schema (T or T:K)".into(),
        ));
        return shm::RELNULL;
    }

    // Step 1: infer the full CSV schema by sampling the file. arrow-rs
    // reads the header, then up to N records to type-sniff each column.
    // The sniff window is bounded so memory stays in check for huge
    // files but large enough to catch promotions like
    // "first row was integer, later rows are floats" -- a common case
    // in real-world CSV.
    //
    // Default: 100 rows. Override with the @MORLOC_CSV_SNIFF_ROWS@
    // environment variable; set it to `0` to disable inference and
    // treat every column as `String` (the safest fallback when the
    // user knows their data has surprising types deep in the file).
    let format = arrow_csv::reader::Format::default()
        .with_header(true)
        .with_delimiter(delimiter);
    let sniff_rows = csv_sniff_rows();
    let mut sniff_cursor = Cursor::new(bytes);
    let inferred_arrow_schema = match format.infer_schema(&mut sniff_cursor, sniff_rows) {
        Ok((s, _records_read)) => s,
        Err(e) => {
            set_errmsg(errmsg, &MorlocError::Other(
                format!("Failed to infer CSV schema: {}", e),
            ));
            return shm::RELNULL;
        }
    };

    // Step 2: merge declared (rs) with inferred. The merged morloc
    // schema dictates both the in-SHM column layout and (after a
    // second mapping below) the typed Arrow schema given to
    // ReaderBuilder.
    let merged_rs = match merge_table_schema_with_csv_header(&rs, &inferred_arrow_schema) {
        Ok(s) => s,
        Err(e) => { set_errmsg(errmsg, &e); return shm::RELNULL; }
    };

    let arrow_schema = match morloc_to_arrow_schema(&merged_rs) {
        Ok(s) => std::sync::Arc::new(s),
        Err(e) => { set_errmsg(errmsg, &e); return shm::RELNULL; }
    };

    let cursor = Cursor::new(bytes);
    let reader = match ReaderBuilder::new(arrow_schema.clone())
        .with_header(true)
        .with_delimiter(delimiter)
        .build(cursor)
    {
        Ok(r) => r,
        Err(e) => {
            set_errmsg(errmsg, &MorlocError::Other(
                format!("Failed to open CSV: {}", e),
            ));
            return shm::RELNULL;
        }
    };
    let batches: Vec<RecordBatch> = match reader.collect::<Result<Vec<_>, _>>() {
        Ok(v) => v,
        Err(e) => {
            set_errmsg(errmsg, &MorlocError::Other(
                format!("Failed to read CSV: {}", e),
            ));
            return shm::RELNULL;
        }
    };

    batches_to_shm(batches, &merged_rs, errmsg)
}

/// Read a Parquet file into the morloc Arrow SHM layout.
///
/// # Safety
/// `data` must point to `data_len` valid bytes. `schema` must be a valid
/// CSchema pointer or null.
#[no_mangle]
pub unsafe extern "C" fn read_parquet_to_shm(
    data: *const u8,
    data_len: usize,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> RelPtr {
    use parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder;
    use bytes::Bytes;

    clear_errmsg(errmsg);
    if data.is_null() || schema.is_null() || data_len == 0 {
        set_errmsg(errmsg, &MorlocError::Other("NULL data or schema".into()));
        return shm::RELNULL;
    }
    let bytes = std::slice::from_raw_parts(data, data_len);
    let rs = CSchema::to_rust(schema);
    if !crate::arrow_ffi::is_arrow_table_schema(&rs) {
        set_errmsg(errmsg, &MorlocError::Other(
            "Parquet reader requires Map+arrow schema".into(),
        ));
        return shm::RELNULL;
    }

    // The parquet reader expects an owned Bytes-like value (it does its own
    // page-level slicing inside). We give it an Arc/copy of the slice; the
    // batch we build later goes into SHM separately.
    let owned = Bytes::copy_from_slice(bytes);
    let builder = match ParquetRecordBatchReaderBuilder::try_new(owned) {
        Ok(b) => b,
        Err(e) => {
            set_errmsg(errmsg, &MorlocError::Other(
                format!("Failed to open Parquet file: {}", e),
            ));
            return shm::RELNULL;
        }
    };
    let reader = match builder.build() {
        Ok(r) => r,
        Err(e) => {
            set_errmsg(errmsg, &MorlocError::Other(
                format!("Failed to build Parquet reader: {}", e),
            ));
            return shm::RELNULL;
        }
    };
    let batches: Vec<RecordBatch> = match reader.collect::<Result<Vec<_>, _>>() {
        Ok(v) => v,
        Err(e) => {
            set_errmsg(errmsg, &MorlocError::Other(
                format!("Failed to read Parquet record batches: {}", e),
            ));
            return shm::RELNULL;
        }
    };

    // Open Table semantics: same alignment+cast pass as Arrow IPC.
    let (aligned, merged_rs) = match align_batches_to_open_schema(batches, &rs) {
        Ok(p) => p,
        Err(e) => { set_errmsg(errmsg, &e); return shm::RELNULL; }
    };

    batches_to_shm(aligned, &merged_rs, errmsg)
}

