//! Arrow IPC / Parquet / CSV readers and writers over the table block
//! layout. Readers decode a file into arrow-rs record batches, then hand
//! them to `arrow_shm::write_batch`, which aligns them with the declared
//! morloc column schema and lays them out; writers do the reverse through
//! `arrow_shm::shm_to_batch`.

use std::ffi::c_char;
use std::io::Cursor;
use std::ptr;
use std::sync::Arc;

use arrow_array::RecordBatch;
use arrow_ipc::reader::{FileReader, StreamReader};
use arrow_schema::{DataType, Field, Schema as ArrowSchema, SchemaRef};

use crate::arrow_ffi::is_arrow_table_schema;
use crate::arrow_shm::{self, ArrowShmHeader};
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
    data.len() >= 8 && &data[..4] == b"PAR1" && &data[data.len() - 4..] == b"PAR1"
}

/// Concatenate a file's batches into one table and lay it out under the
/// declared schema. A file with a schema but no batches is a zero-row
/// table of that schema.
unsafe fn batches_to_shm(
    batches: Vec<RecordBatch>,
    file_schema: SchemaRef,
    rs: &Schema,
    errmsg: *mut *mut c_char,
) -> RelPtr {
    let batch = if batches.is_empty() {
        RecordBatch::new_empty(file_schema)
    } else if batches.len() == 1 {
        batches.into_iter().next().unwrap()
    } else {
        match arrow_select::concat::concat_batches(&file_schema, &batches) {
            Ok(b) => b,
            Err(e) => {
                set_errmsg(errmsg, &MorlocError::Other(format!("Failed to concatenate batches: {}", e)));
                return shm::RELNULL;
            }
        }
    };
    match arrow_shm::write_batch(&batch, Some(rs)) {
        Ok(r) => r,
        Err(e) => {
            set_errmsg(errmsg, &e);
            shm::RELNULL
        }
    }
}

/// Copy a byte vector into a libc-allocated buffer the caller frees.
unsafe fn hand_out(buf: Vec<u8>, out_buf: *mut *mut u8, out_len: *mut usize, errmsg: *mut *mut c_char) -> i32 {
    let len = buf.len();
    let mem = libc::malloc(len.max(1)) as *mut u8;
    if mem.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("malloc failed".into()));
        return 1;
    }
    ptr::copy_nonoverlapping(buf.as_ptr(), mem, len);
    *out_buf = mem;
    *out_len = len;
    0
}

/// Serialize a table block to Arrow IPC file bytes. On success `*out_buf`
/// is a libc-allocated buffer of `*out_len` bytes the caller must
/// `libc::free`; on error returns nonzero with `errmsg` set.
///
/// # Safety
/// `header` must be a live table block.
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
    let batch = match arrow_shm::shm_to_batch(header) {
        Ok(b) => b,
        Err(e) => {
            set_errmsg(errmsg, &e);
            return 1;
        }
    };
    let mut buf: Vec<u8> = Vec::new();
    {
        let mut writer = match FileWriter::try_new(&mut buf, &batch.schema()) {
            Ok(w) => w,
            Err(e) => {
                set_errmsg(errmsg, &MorlocError::Other(format!("FileWriter::try_new: {}", e)));
                return 1;
            }
        };
        if let Err(e) = writer.write(&batch) {
            set_errmsg(errmsg, &MorlocError::Other(format!("Arrow IPC write: {}", e)));
            return 1;
        }
        if let Err(e) = writer.finish() {
            set_errmsg(errmsg, &MorlocError::Other(format!("Arrow IPC finish: {}", e)));
            return 1;
        }
    }
    hand_out(buf, out_buf, out_len, errmsg)
}

/// Serialize a table block to Parquet bytes. See
/// `write_arrow_ipc_to_buffer` for the buffer ownership convention.
///
/// # Safety
/// `header` must be a live table block.
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
    let batch = match arrow_shm::shm_to_batch(header) {
        Ok(b) => b,
        Err(e) => {
            set_errmsg(errmsg, &e);
            return 1;
        }
    };
    let mut buf: Vec<u8> = Vec::new();
    {
        let mut writer = match ArrowWriter::try_new(&mut buf, batch.schema(), None) {
            Ok(w) => w,
            Err(e) => {
                set_errmsg(errmsg, &MorlocError::Other(format!("ArrowWriter::try_new: {}", e)));
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
    hand_out(buf, out_buf, out_len, errmsg)
}

/// Serialize a table block to delimited text with a header line. See
/// `write_arrow_ipc_to_buffer` for the buffer ownership convention.
///
/// # Safety
/// `header` must be a live table block.
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
    let batch = match arrow_shm::shm_to_batch(header) {
        Ok(b) => b,
        Err(e) => {
            set_errmsg(errmsg, &e);
            return 1;
        }
    };
    let mut buf: Vec<u8> = Vec::new();
    {
        let mut writer = WriterBuilder::new().with_header(true).with_delimiter(delimiter).build(&mut buf);
        if let Err(e) = writer.write(&batch) {
            set_errmsg(errmsg, &MorlocError::Other(format!("CSV write: {}", e)));
            return 1;
        }
    }
    hand_out(buf, out_buf, out_len, errmsg)
}

/// Read an Arrow IPC file (ARROW1 magic) or stream into a table block
/// under the declared schema.
///
/// # Safety
/// `data` must point to `data_len` valid bytes; `schema` must be a valid
/// CSchema pointer.
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
    if !is_arrow_table_schema(&rs) {
        set_errmsg(errmsg, &MorlocError::Other("Arrow IPC reader requires a Table schema".into()));
        return shm::RELNULL;
    }

    let (file_schema, batches) = if is_arrow_file_magic(bytes) {
        let reader = match FileReader::try_new(Cursor::new(bytes), None) {
            Ok(r) => r,
            Err(e) => {
                set_errmsg(errmsg, &MorlocError::Other(format!("Failed to open Arrow IPC file: {}", e)));
                return shm::RELNULL;
            }
        };
        let s = reader.schema();
        match reader.collect::<Result<Vec<_>, _>>() {
            Ok(v) => (s, v),
            Err(e) => {
                set_errmsg(errmsg, &MorlocError::Other(format!("Failed to read Arrow IPC batches: {}", e)));
                return shm::RELNULL;
            }
        }
    } else {
        let reader = match StreamReader::try_new(Cursor::new(bytes), None) {
            Ok(r) => r,
            Err(e) => {
                set_errmsg(errmsg, &MorlocError::Other(format!("Failed to open Arrow IPC stream: {}", e)));
                return shm::RELNULL;
            }
        };
        let s = reader.schema();
        match reader.collect::<Result<Vec<_>, _>>() {
            Ok(v) => (s, v),
            Err(e) => {
                set_errmsg(errmsg, &MorlocError::Other(format!("Failed to read Arrow IPC stream batches: {}", e)));
                return shm::RELNULL;
            }
        }
    };
    batches_to_shm(batches, file_schema, &rs, errmsg)
}

/// The Arrow type a declared morloc column parses as. CSV cells are
/// parsed straight into the declared type, so this is also the parser's
/// instruction.
fn declared_field(key: &str, p: &Schema) -> Result<Field, MorlocError> {
    let (inner, nullable) = if p.serial_type == SerialType::Optional {
        (p.parameters.first().map(|c| c.serial_type).unwrap_or(SerialType::Nil), true)
    } else {
        (p.serial_type, false)
    };
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
        other => {
            return Err(MorlocError::Other(format!(
                "Unsupported column type for '{}': {:?}",
                key, other
            )))
        }
    };
    Ok(Field::new(key, dt, nullable))
}

/// The CSV parse type for a column the declaration does not mention:
/// arrow-csv's inference for the numeric and text families, and the raw
/// cell text for anything else, which round-trips without lossy coercion.
fn inferred_csv_type(dt: &DataType) -> DataType {
    match dt {
        DataType::Boolean
        | DataType::Int8
        | DataType::Int16
        | DataType::Int32
        | DataType::Int64
        | DataType::UInt8
        | DataType::UInt16
        | DataType::UInt32
        | DataType::UInt64
        | DataType::Float32
        | DataType::Float64
        | DataType::Utf8
        | DataType::LargeUtf8 => dt.clone(),
        _ => DataType::Utf8,
    }
}

/// The schema a CSV is parsed with: declared columns first, parsed as
/// declared; the header's other columns after, parsed as inferred and
/// nullable. A declared column absent from the header is an error.
fn csv_parse_schema(rs: &Schema, inferred: &ArrowSchema) -> Result<ArrowSchema, MorlocError> {
    let mut fields: Vec<Field> = Vec::with_capacity(inferred.fields().len());
    for (k, p) in rs.keys.iter().zip(rs.parameters.iter()) {
        if inferred.index_of(k).is_err() {
            return Err(MorlocError::Other(format!("Declared column '{}' missing from CSV header", k)));
        }
        fields.push(declared_field(k, p)?);
    }
    let declared: std::collections::HashSet<&str> = rs.keys.iter().map(|s| s.as_str()).collect();
    for f in inferred.fields() {
        if declared.contains(f.name().as_str()) {
            continue;
        }
        fields.push(Field::new(f.name(), inferred_csv_type(f.data_type()), true));
    }
    Ok(ArrowSchema::new(fields))
}

/// Resolve the sniff-window size for CSV type inference: 100 records by
/// default, overridden by `MORLOC_CSV_SNIFF_ROWS` (0 disables inference
/// and reads every column as text). Never `None`, which would scan the
/// whole input.
fn csv_sniff_rows() -> Option<usize> {
    const DEFAULT: usize = 100;
    match std::env::var("MORLOC_CSV_SNIFF_ROWS") {
        Ok(s) => Some(s.parse::<usize>().unwrap_or(DEFAULT)),
        Err(_) => Some(DEFAULT),
    }
}

/// Map an arrow DataType to the short category morloc-nexus's `file`
/// surface displays.
fn arrow_type_category(dt: &DataType) -> &'static str {
    use arrow_schema::DataType::*;
    match dt {
        Int8 | Int16 | Int32 | Int64 | UInt8 | UInt16 | UInt32 | UInt64 => "int",
        Float16 | Float32 | Float64 => "float",
        Utf8 | LargeUtf8 | Utf8View => "str",
        Boolean => "bool",
        Date32 | Date64 => "date",
        Time32(_) | Time64(_) | Timestamp(_, _) | Duration(_) | Interval(_) => "time",
        _ => "other",
    }
}

/// Dry-run CSV / TSV inference: confirms the bytes parse as a valid CSV
/// under the given delimiter and reports the inferred column schema as a
/// libc-allocated JSON string `{"columns":[{"name":..,"type":..}, ...]}`
/// the caller frees. Used by `morloc-nexus file` so a file it classifies
/// as CSV is one `view`/`run` can also read.
///
/// # Safety
/// `data` must point to `data_len` valid bytes; `out_info` and `errmsg`
/// must be writable.
#[no_mangle]
pub unsafe extern "C" fn morloc_csv_infer(
    data: *const u8,
    data_len: usize,
    delimiter: u8,
    out_info: *mut *mut c_char,
    errmsg: *mut *mut c_char,
) -> bool {
    clear_errmsg(errmsg);
    *out_info = ptr::null_mut();
    if data.is_null() || data_len == 0 {
        set_errmsg(errmsg, &MorlocError::Other("empty input".into()));
        return false;
    }
    let bytes = std::slice::from_raw_parts(data, data_len);
    let format = arrow_csv::reader::Format::default().with_header(true).with_delimiter(delimiter);
    let mut cursor = Cursor::new(bytes);
    let schema = match format.infer_schema(&mut cursor, csv_sniff_rows()) {
        Ok((s, _)) => s,
        Err(e) => {
            set_errmsg(errmsg, &MorlocError::Other(format!("{}", e)));
            return false;
        }
    };
    let columns: Vec<serde_json::Value> = schema
        .fields()
        .iter()
        .map(|f| serde_json::json!({ "name": f.name(), "type": arrow_type_category(f.data_type()) }))
        .collect();
    let json = serde_json::json!({ "columns": columns }).to_string();
    let bytes = json.as_bytes();
    let buf = libc::malloc(bytes.len() + 1) as *mut u8;
    if buf.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("malloc failed for csv info".into()));
        return false;
    }
    ptr::copy_nonoverlapping(bytes.as_ptr(), buf, bytes.len());
    *buf.add(bytes.len()) = 0;
    *out_info = buf as *mut c_char;
    true
}

/// Read a CSV / TSV file into a table block. The declared columns are
/// parsed as declared; the header's other columns are appended with
/// inferred types.
///
/// # Safety
/// `data` must point to `data_len` valid bytes; `schema` must be a valid
/// CSchema pointer.
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
    if !is_arrow_table_schema(&rs) {
        set_errmsg(errmsg, &MorlocError::Other("CSV reader requires a Table schema".into()));
        return shm::RELNULL;
    }

    let format = arrow_csv::reader::Format::default().with_header(true).with_delimiter(delimiter);
    let mut sniff_cursor = Cursor::new(bytes);
    let inferred = match format.infer_schema(&mut sniff_cursor, csv_sniff_rows()) {
        Ok((s, _)) => s,
        Err(e) => {
            set_errmsg(errmsg, &MorlocError::Other(format!("Failed to infer CSV schema: {}", e)));
            return shm::RELNULL;
        }
    };
    let parse_schema = match csv_parse_schema(&rs, &inferred) {
        Ok(s) => Arc::new(s),
        Err(e) => {
            set_errmsg(errmsg, &e);
            return shm::RELNULL;
        }
    };

    let reader = match ReaderBuilder::new(parse_schema.clone())
        .with_header(true)
        .with_delimiter(delimiter)
        .build(Cursor::new(bytes))
    {
        Ok(r) => r,
        Err(e) => {
            set_errmsg(errmsg, &MorlocError::Other(format!("Failed to open CSV: {}", e)));
            return shm::RELNULL;
        }
    };
    let batches: Vec<RecordBatch> = match reader.collect::<Result<Vec<_>, _>>() {
        Ok(v) => v,
        Err(e) => {
            set_errmsg(errmsg, &MorlocError::Other(format!("Failed to read CSV: {}", e)));
            return shm::RELNULL;
        }
    };
    batches_to_shm(batches, parse_schema, &rs, errmsg)
}

/// Read a Parquet file into a table block under the declared schema.
///
/// # Safety
/// `data` must point to `data_len` valid bytes; `schema` must be a valid
/// CSchema pointer.
#[no_mangle]
pub unsafe extern "C" fn read_parquet_to_shm(
    data: *const u8,
    data_len: usize,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> RelPtr {
    use bytes::Bytes;
    use parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder;

    clear_errmsg(errmsg);
    if data.is_null() || schema.is_null() || data_len == 0 {
        set_errmsg(errmsg, &MorlocError::Other("NULL data or schema".into()));
        return shm::RELNULL;
    }
    let bytes = std::slice::from_raw_parts(data, data_len);
    let rs = CSchema::to_rust(schema);
    if !is_arrow_table_schema(&rs) {
        set_errmsg(errmsg, &MorlocError::Other("Parquet reader requires a Table schema".into()));
        return shm::RELNULL;
    }

    let owned = Bytes::copy_from_slice(bytes);
    let builder = match ParquetRecordBatchReaderBuilder::try_new(owned) {
        Ok(b) => b,
        Err(e) => {
            set_errmsg(errmsg, &MorlocError::Other(format!("Failed to open Parquet file: {}", e)));
            return shm::RELNULL;
        }
    };
    let file_schema = builder.schema().clone();
    let reader = match builder.build() {
        Ok(r) => r,
        Err(e) => {
            set_errmsg(errmsg, &MorlocError::Other(format!("Failed to build Parquet reader: {}", e)));
            return shm::RELNULL;
        }
    };
    let batches: Vec<RecordBatch> = match reader.collect::<Result<Vec<_>, _>>() {
        Ok(v) => v,
        Err(e) => {
            set_errmsg(errmsg, &MorlocError::Other(format!("Failed to read Parquet record batches: {}", e)));
            return shm::RELNULL;
        }
    };
    batches_to_shm(batches, file_schema, &rs, errmsg)
}
