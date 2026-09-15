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
use arrow_schema::{DataType, Field, Schema as ArrowSchema, SchemaRef};

use crate::arrow_ffi::is_arrow_table_schema;
use crate::arrow_shm::{self, ArrowShmHeader};
use crate::cschema::CSchema;
use crate::error::{set_errmsg, MorlocError};
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
    crate::error::guarded(errmsg, 1, || write_arrow_ipc_to_buffer_impl(header, out_buf, out_len, errmsg))
}

unsafe fn write_arrow_ipc_to_buffer_impl(
    header: *const ArrowShmHeader,
    out_buf: *mut *mut u8,
    out_len: *mut usize,
    errmsg: *mut *mut c_char,
) -> i32 {
    use arrow_ipc::writer::FileWriter;
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
    crate::error::guarded(errmsg, 1, || write_parquet_to_buffer_impl(header, out_buf, out_len, errmsg))
}

unsafe fn write_parquet_to_buffer_impl(
    header: *const ArrowShmHeader,
    out_buf: *mut *mut u8,
    out_len: *mut usize,
    errmsg: *mut *mut c_char,
) -> i32 {
    use parquet::arrow::ArrowWriter;
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
    crate::error::guarded(errmsg, 1, || write_csv_to_buffer_impl(header, delimiter, out_buf, out_len, errmsg))
}

unsafe fn write_csv_to_buffer_impl(
    header: *const ArrowShmHeader,
    delimiter: u8,
    out_buf: *mut *mut u8,
    out_len: *mut usize,
    errmsg: *mut *mut c_char,
) -> i32 {
    use arrow_csv::writer::WriterBuilder;
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
    crate::error::guarded(errmsg, shm::RELNULL, || read_arrow_ipc_to_shm_impl(data, data_len, schema, errmsg))
}

unsafe fn read_arrow_ipc_to_shm_impl(
    data: *const u8,
    data_len: usize,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> RelPtr {
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

    let decoded = if is_arrow_file_magic(bytes) { read_ipc_file(bytes) } else { read_ipc_stream(bytes) };
    match decoded {
        Ok((file_schema, batches)) => batches_to_shm(batches, file_schema, &rs, errmsg),
        Err(e) => {
            set_errmsg(errmsg, &e);
            shm::RELNULL
        }
    }
}

/// Decode an Arrow IPC payload into a fresh block and return its absolute
/// address, or NULL with `errmsg` set: the shape a packet reader wants.
///
/// # Safety
/// As `read_arrow_ipc_to_shm`.
pub unsafe fn ipc_payload_to_block(
    data: *const u8,
    data_len: usize,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut std::ffi::c_void {
    let rel = read_arrow_ipc_to_shm(data, data_len, schema, errmsg);
    if rel == shm::RELNULL {
        return ptr::null_mut();
    }
    match shm::rel2abs(rel) {
        Ok(abs) => abs as *mut std::ffi::c_void,
        Err(e) => {
            set_errmsg(errmsg, &e);
            ptr::null_mut()
        }
    }
}

/// Largest decompressed size a compressed IPC buffer may announce. The
/// decoder allocates the announced size before it reads a byte, and an
/// allocation that fails ends the process, so the announcement is checked
/// against this bound first. Four gibibytes is the reach of the 32-bit
/// offsets most columns use and is allocatable wherever such a table
/// could be decoded at all.
const MAX_DECOMPRESSED_BUFFER: i64 = 1 << 32;

/// Refuse a compressed record batch whose buffers announce more than
/// `MAX_DECOMPRESSED_BUFFER` bytes each, before the decoder trusts them.
fn check_compressed_sizes(batch: &arrow_ipc::RecordBatch<'_>, body: &[u8]) -> Result<(), MorlocError> {
    if batch.compression().is_none() {
        return Ok(());
    }
    for b in batch.buffers().iter().flatten() {
        let off = usize::try_from(b.offset()).map_err(|_| MorlocError::Other("negative IPC buffer offset".into()))?;
        let len = usize::try_from(b.length()).map_err(|_| MorlocError::Other("negative IPC buffer length".into()))?;
        off.checked_add(len)
            .filter(|&e| e <= body.len())
            .ok_or_else(|| MorlocError::Other("IPC buffer lies outside the message body".into()))?;
        if len >= 8 {
            let announced = i64::from_le_bytes(body[off..off + 8].try_into().unwrap());
            if announced > MAX_DECOMPRESSED_BUFFER {
                return Err(MorlocError::Other(format!(
                    "IPC buffer announces {} decompressed bytes, more than the {} this reader accepts",
                    announced, MAX_DECOMPRESSED_BUFFER
                )));
            }
        }
    }
    Ok(())
}

/// The flatbuffer inside an encapsulated message: the optional
/// continuation marker and the length prefix stripped.
fn message_flatbuffer(meta: &[u8]) -> &[u8] {
    if meta.len() >= 8 && meta[..4] == [0xFF, 0xFF, 0xFF, 0xFF] {
        &meta[8..]
    } else if meta.len() >= 4 {
        &meta[4..]
    } else {
        &[]
    }
}

/// Check the compressed sizes a message's batch announces for its body.
fn check_message(message: &arrow_ipc::Message<'_>, body: &[u8]) -> Result<(), MorlocError> {
    if let Some(rb) = message.header_as_record_batch() {
        check_compressed_sizes(&rb, body)?;
    }
    if let Some(rb) = message.header_as_dictionary_batch().and_then(|d| d.data()) {
        check_compressed_sizes(&rb, body)?;
    }
    Ok(())
}

/// Decode an Arrow IPC file. Every block is bounds-checked against the
/// bytes in hand before the decoder sees it: the footer's block table is
/// what a damaged file lies about, and the stock reader allocates each
/// block's announced size before reading it.
fn read_ipc_file(bytes: &[u8]) -> Result<(SchemaRef, Vec<RecordBatch>), MorlocError> {
    use arrow_buffer::Buffer;
    use arrow_ipc::reader::{read_footer_length, FileDecoder};
    let bad = |what: String| MorlocError::Other(format!("Failed to open Arrow IPC file: {}", what));
    if bytes.len() < 10 {
        return Err(bad("file is shorter than its trailer".into()));
    }
    let trailer_start = bytes.len() - 10;
    let footer_len = read_footer_length(bytes[trailer_start..].try_into().unwrap()).map_err(|e| bad(e.to_string()))?;
    let footer_start = trailer_start
        .checked_sub(footer_len)
        .ok_or_else(|| bad("footer length exceeds the file".into()))?;
    let footer = arrow_ipc::root_as_footer(&bytes[footer_start..trailer_start]).map_err(|e| bad(format!("{:?}", e)))?;
    let schema = Arc::new(arrow_ipc::convert::fb_to_schema(
        footer.schema().ok_or_else(|| bad("footer carries no schema".into()))?,
    ));
    let buffer = Buffer::from(bytes);
    // A block's bytes, bounds-checked and with its announced compressed
    // sizes checked, before the decoder sees them.
    let checked_block = |block: &arrow_ipc::Block| -> Result<Buffer, MorlocError> {
        let off = usize::try_from(block.offset()).map_err(|_| bad("negative block offset".into()))?;
        let meta = usize::try_from(block.metaDataLength()).map_err(|_| bad("negative block metadata length".into()))?;
        let body = usize::try_from(block.bodyLength()).map_err(|_| bad("negative block body length".into()))?;
        let len = meta.checked_add(body).ok_or_else(|| bad("block length overflows".into()))?;
        let end = off.checked_add(len).ok_or_else(|| bad("block end overflows".into()))?;
        if end > bytes.len() {
            return Err(bad("a block lies outside the file".into()));
        }
        let data = buffer.slice_with_length(off, len);
        let (meta_bytes, body_bytes) = data.split_at(meta);
        if let Ok(message) = arrow_ipc::root_as_message(message_flatbuffer(meta_bytes)) {
            check_message(&message, body_bytes)?;
        }
        Ok(data)
    };
    let mut decoder = FileDecoder::new(schema.clone(), footer.version());
    for block in footer.dictionaries().iter().flatten() {
        let data = checked_block(block)?;
        decoder.read_dictionary(block, &data).map_err(|e| bad(format!("dictionary: {}", e)))?;
    }
    let mut batches = Vec::new();
    for block in footer.recordBatches().iter().flatten() {
        let data = checked_block(block)?;
        if let Some(b) = decoder
            .read_record_batch(block, &data)
            .map_err(|e| MorlocError::Other(format!("Failed to read Arrow IPC batches: {}", e)))?
        {
            batches.push(b);
        }
    }
    Ok((schema, batches))
}

/// Decode an Arrow IPC stream. The stream decoder only ever copies bytes
/// it has, so the one thing to check ahead of it is what compressed
/// buffers announce.
fn read_ipc_stream(bytes: &[u8]) -> Result<(SchemaRef, Vec<RecordBatch>), MorlocError> {
    use arrow_buffer::Buffer;
    use arrow_ipc::reader::StreamDecoder;
    let bad = |what: String| MorlocError::Other(format!("Failed to read Arrow IPC stream: {}", what));
    let mut pos = 0usize;
    let u32_at = |at: usize| -> Option<u32> { bytes.get(at..at + 4).map(|b| u32::from_le_bytes(b.try_into().unwrap())) };
    loop {
        let mut meta_len = match u32_at(pos) { Some(n) => n, None => break };
        pos += 4;
        if meta_len == 0xFFFF_FFFF {
            meta_len = match u32_at(pos) { Some(n) => n, None => break };
            pos += 4;
        }
        if meta_len == 0 {
            break;
        }
        let meta_end = match pos.checked_add(meta_len as usize).filter(|&e| e <= bytes.len()) { Some(e) => e, None => break };
        let message = match arrow_ipc::root_as_message(&bytes[pos..meta_end]) { Ok(m) => m, Err(_) => break };
        let body_end = match usize::try_from(message.bodyLength()).ok().and_then(|n| meta_end.checked_add(n)).filter(|&e| e <= bytes.len()) {
            Some(e) => e,
            None => break,
        };
        check_message(&message, &bytes[meta_end..body_end])?;
        pos = body_end;
    }
    let mut decoder = StreamDecoder::new();
    let mut buffer = Buffer::from(bytes);
    let mut batches = Vec::new();
    while let Some(b) = decoder.decode(&mut buffer).map_err(|e| bad(e.to_string()))? {
        batches.push(b);
    }
    decoder.finish().map_err(|e| bad(e.to_string()))?;
    let schema = decoder.schema().ok_or_else(|| bad("stream carries no schema".into()))?;
    Ok((schema, batches))
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

/// The schema a CSV is parsed with: the header's columns in header order,
/// since the parser binds fields to columns by position. A declared
/// column is parsed as declared; any other is parsed as inferred and
/// nullable. A declared column absent from the header is an error. The
/// declared-first order the block carries is applied afterwards by
/// `align_to_declared`.
fn csv_parse_schema(rs: &Schema, inferred: &ArrowSchema) -> Result<ArrowSchema, MorlocError> {
    for k in rs.keys.iter() {
        if inferred.index_of(k).is_err() {
            return Err(MorlocError::Other(format!("Declared column '{}' missing from CSV header", k)));
        }
    }
    let mut fields: Vec<Field> = Vec::with_capacity(inferred.fields().len());
    for f in inferred.fields() {
        match rs.keys.iter().position(|k| k == f.name()) {
            Some(i) => fields.push(declared_field(f.name(), &rs.parameters[i])?),
            None => fields.push(Field::new(f.name(), inferred_csv_type(f.data_type()), true)),
        }
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
    crate::error::guarded(errmsg, false, || morloc_csv_infer_impl(data, data_len, delimiter, out_info, errmsg))
}

unsafe fn morloc_csv_infer_impl(
    data: *const u8,
    data_len: usize,
    delimiter: u8,
    out_info: *mut *mut c_char,
    errmsg: *mut *mut c_char,
) -> bool {
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
    crate::error::guarded(errmsg, shm::RELNULL, || read_csv_to_shm_impl(data, data_len, delimiter, schema, errmsg))
}

unsafe fn read_csv_to_shm_impl(
    data: *const u8,
    data_len: usize,
    delimiter: u8,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> RelPtr {
    use arrow_csv::ReaderBuilder;

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
    crate::error::guarded(errmsg, shm::RELNULL, || read_parquet_to_shm_impl(data, data_len, schema, errmsg))
}

unsafe fn read_parquet_to_shm_impl(
    data: *const u8,
    data_len: usize,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> RelPtr {
    use bytes::Bytes;
    use parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder;

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

#[cfg(test)]
mod tests {
    use super::*;
    use arrow_array::{Int64Array, StringArray};
    use arrow_schema::Field;

    fn fixture() -> RecordBatch {
        let schema = ArrowSchema::new(vec![
            Field::new("x", DataType::Int64, true),
            Field::new("s", DataType::Utf8, true),
        ]);
        RecordBatch::try_new(
            Arc::new(schema),
            vec![
                Arc::new(Int64Array::from(vec![Some(1), None, Some(3)])),
                Arc::new(StringArray::from(vec![Some("a"), None, Some("ccc")])),
            ],
        )
        .unwrap()
    }

    /// A table schema declaring the given columns, as the C ABI takes it.
    fn declared(cols: &[(&str, SerialType)]) -> *mut CSchema {
        CSchema::from_rust(&Schema::table(
            cols.iter().map(|(_, t)| Schema::primitive(*t)).collect(),
            cols.iter().map(|(k, _)| k.to_string()).collect(),
        ))
    }

    fn ipc_bytes() -> Vec<u8> {
        let batch = fixture();
        let mut buf = Vec::new();
        let mut w = arrow_ipc::writer::FileWriter::try_new(&mut buf, &batch.schema()).unwrap();
        w.write(&batch).unwrap();
        w.finish().unwrap();
        buf
    }

    fn parquet_bytes() -> Vec<u8> {
        let batch = fixture();
        let mut buf = Vec::new();
        let mut w = parquet::arrow::ArrowWriter::try_new(&mut buf, batch.schema(), None).unwrap();
        w.write(&batch).unwrap();
        w.close().unwrap();
        buf
    }

    /// Every outcome of a reader over a damaged file is an error or a
    /// table, never an abort. The readers run under a C ABI where an
    /// unwinding panic ends the process, so this test is its own witness:
    /// an unguarded decoder panic kills the test binary.
    fn survives_mutations(name: &str, valid: &[u8], read: &dyn Fn(&[u8], *const CSchema, *mut *mut c_char) -> RelPtr) {
        let _guard = crate::init_test_shm();
        let schema = declared(&[]);
        let mut seed: u64 = 0x9E3779B97F4A7C15;
        let mut next = || {
            seed ^= seed << 13;
            seed ^= seed >> 7;
            seed ^= seed << 17;
            seed
        };
        let mut errors = 0usize;
        let rounds: usize = std::env::var("MORLOC_TEST_MUTATIONS").ok().and_then(|s| s.parse().ok()).unwrap_or(400);
        for _ in 0..rounds {
            let mut bytes = valid.to_vec();
            let flips = 1 + (next() % 3) as usize;
            for _ in 0..flips {
                let at = (next() as usize) % bytes.len();
                bytes[at] ^= (next() % 255 + 1) as u8;
            }
            let mut err: *mut c_char = ptr::null_mut();
            let rel = read(&bytes, schema, &mut err);
            if rel == shm::RELNULL {
                assert!(!err.is_null(), "{}: a refused file must carry a message", name);
                unsafe { libc::free(err as *mut libc::c_void) };
                errors += 1;
            } else {
                assert!(err.is_null());
                let base = shm::rel2abs(rel).unwrap();
                assert!(unsafe { arrow_shm::BlockView::open(base as *const ArrowShmHeader) }.is_ok());
                let _ = shm::shfree(base);
            }
        }
        assert!(errors > 0, "{}: no mutation was refused", name);
        unsafe { CSchema::free(schema) };
    }

    #[test]
    fn damaged_arrow_ipc_is_refused_not_fatal() {
        survives_mutations("arrow ipc", &ipc_bytes(), &|b, s, e| unsafe {
            read_arrow_ipc_to_shm(b.as_ptr(), b.len(), s, e)
        });
    }

    #[test]
    fn damaged_parquet_is_refused_not_fatal() {
        survives_mutations("parquet", &parquet_bytes(), &|b, s, e| unsafe {
            read_parquet_to_shm(b.as_ptr(), b.len(), s, e)
        });
    }

    fn read_csv(text: &[u8], schema: *mut CSchema) -> RecordBatch {
        let mut err: *mut c_char = ptr::null_mut();
        let rel = unsafe { read_csv_to_shm(text.as_ptr(), text.len(), b',', schema, &mut err) };
        assert!(err.is_null(), "{}", unsafe { std::ffi::CStr::from_ptr(err) }.to_string_lossy());
        let base = shm::rel2abs(rel).unwrap();
        unsafe { arrow_shm::shm_to_batch(base as *const ArrowShmHeader) }.unwrap()
    }

    #[test]
    fn csv_columns_bind_by_header_name_not_position() {
        let _guard = crate::init_test_shm();
        // Header order opposite to the declaration.
        let schema = declared(&[("age", SerialType::Sint64), ("name", SerialType::String)]);
        let back = read_csv(b"name,age\nalice,30\nbob,41\n", schema);
        unsafe { CSchema::free(schema) };
        assert_eq!(back.schema().field(0).name(), "age");
        assert_eq!(back.schema().field(1).name(), "name");
        let age = back.column(0).as_any().downcast_ref::<Int64Array>().unwrap();
        assert_eq!(age.values(), &[30, 41]);
        let name = back.column(1).as_any().downcast_ref::<StringArray>().unwrap();
        assert_eq!(name.value(0), "alice");

        // Two same-typed columns: a positional binding would swap them
        // without an error.
        let schema = declared(&[("last", SerialType::String), ("first", SerialType::String)]);
        let back = read_csv(b"first,last\nada,lovelace\n", schema);
        unsafe { CSchema::free(schema) };
        assert_eq!(back.schema().field(0).name(), "last");
        let last = back.column(0).as_any().downcast_ref::<StringArray>().unwrap();
        assert_eq!(last.value(0), "lovelace");
        let first = back.column(1).as_any().downcast_ref::<StringArray>().unwrap();
        assert_eq!(first.value(0), "ada");

        // An undeclared column keeps its header position after the
        // declared ones.
        let schema = declared(&[("y", SerialType::Sint64)]);
        let back = read_csv(b"x,y,z\n1,2,3\n", schema);
        unsafe { CSchema::free(schema) };
        let schema_back = back.schema();
        let names: Vec<&str> = schema_back.fields().iter().map(|f| f.name().as_str()).collect();
        assert_eq!(names, ["y", "x", "z"]);
    }

    #[test]
    fn damaged_csv_is_refused_not_fatal() {
        let csv = b"x,s\n1,a\n2,\"b,b\"\n3,ccc\n";
        survives_mutations("csv", csv, &|b, s, e| unsafe {
            read_csv_to_shm(b.as_ptr(), b.len(), b',', s, e)
        });
    }
}
