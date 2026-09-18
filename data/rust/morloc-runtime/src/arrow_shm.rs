//! Arrow tables in shared memory.
//!
//! A table is one Arrow struct array (a record batch) stored as a descriptor
//! table over opaque byte buffers. The layer never interprets a buffer's
//! contents: it records how many buffers each array node has and how long
//! each is, and the consumer's Arrow library derives their meaning from the
//! node's C Data Interface format string, exactly as it would for any
//! imported array. The reader does check, once per block, that every
//! node's length is covered by the buffers it names, because the C Data
//! Interface carries no buffer lengths and every consumer recomputes them
//! from the length.
//!
//! Block layout, all offsets relative to the block start:
//!
//! ```text
//! [0)              ArrowShmHeader           64 bytes
//! [nodes_offset)   ArrowNodeDesc[n_nodes]   node 0 is the root struct; the
//!                                           children of node k occupy the
//!                                           contiguous range starting at
//!                                           child_index, always > k
//! [buffers_offset) ArrowBufferDesc[n_buffers]
//! [strtab_offset)  string table: format strings, names and per-node Arrow
//!                  metadata blobs, NUL-terminated; strtab[0] is NUL so an
//!                  offset of 0 means "absent"
//! [metadata_offset) msgpack map of morloc.* keys, or absent
//! [data_start)     buffer bytes, each 64-byte aligned from the block start
//! ```
//!
//! Every buffer is `BUF_LOCAL` (bytes in this block) or `BUF_NULL` (the
//! slot the C Data Interface leaves as a null pointer, e.g. an absent
//! validity bitmap). `BUF_EXTERN` is reserved for aliasing a buffer in
//! another block and is rejected by the reader.

use std::ffi::{c_char, c_void, CStr};
use std::ptr;
use std::sync::Arc;

use arrow_array::ffi::{from_ffi, FFI_ArrowArray, FFI_ArrowSchema};
use arrow_array::{Array, ArrayRef, RecordBatch, StructArray};
use arrow_data::transform::MutableArrayData;
use arrow_data::{layout, ArrayData};
use arrow_schema::{DataType, Field, Schema as ArrowSchema, SchemaRef};

use crate::error::MorlocError;
use crate::schema::{Schema, SerialType};
use crate::shm::{self, RelPtr};

pub const ARROW_SHM_MAGIC: u32 = 0xA770DA7A;
pub const ARROW_SHM_VERSION: u32 = 2;
pub const ARROW_BUFFER_ALIGN: usize = 64;

pub const BUF_NULL: u32 = 0;
pub const BUF_LOCAL: u32 = 1;
pub const BUF_EXTERN: u32 = 2;

pub const ARROW_FLAG_DICTIONARY_ORDERED: i64 = 1;
pub const ARROW_FLAG_NULLABLE: i64 = 2;
pub const ARROW_FLAG_MAP_KEYS_SORTED: i64 = 4;

pub fn align_up(x: usize) -> usize {
    (x + ARROW_BUFFER_ALIGN - 1) & !(ARROW_BUFFER_ALIGN - 1)
}

#[repr(C)]
pub struct ArrowShmHeader {
    pub magic: u32,
    pub version: u32,
    pub n_columns: u32,
    pub n_nodes: u32,
    pub n_rows: u64,
    pub total_size: u64,
    pub metadata_offset: u64,
    pub metadata_length: u32,
    pub n_buffers: u32,
    pub nodes_offset: u32,
    pub buffers_offset: u32,
    pub strtab_offset: u32,
    pub strtab_length: u32,
}

#[repr(C)]
pub struct ArrowNodeDesc {
    pub length: i64,
    pub null_count: i64,
    pub offset: i64,
    pub flags: i64,
    pub format_offset: u32,
    pub name_offset: u32,
    pub metadata_offset: u32,
    pub buffer_index: u32,
    pub n_buffers: u32,
    pub child_index: u32,
    pub n_children: u32,
    pub dictionary_index: u32,
}

#[repr(C)]
pub struct ArrowBufferDesc {
    pub size: u64,
    pub offset: u64,
    pub extern_ref: u64,
    pub kind: u32,
    pub _pad: u32,
}

const _: () = assert!(std::mem::size_of::<ArrowShmHeader>() == 64);
const _: () = assert!(std::mem::size_of::<ArrowNodeDesc>() == 64);
const _: () = assert!(std::mem::size_of::<ArrowBufferDesc>() == 32);
const _: () = assert!(std::mem::align_of::<ArrowShmHeader>() == 8);
const _: () = assert!(std::mem::align_of::<ArrowNodeDesc>() == 8);
const _: () = assert!(std::mem::align_of::<ArrowBufferDesc>() == 8);

fn err<S: Into<String>>(s: S) -> MorlocError {
    MorlocError::Other(s.into())
}

fn arrow_err(what: &str, e: arrow_schema::ArrowError) -> MorlocError {
    err(format!("{}: {}", what, e))
}

// -- Producer-side normalisation ---------------------------------------------

/// True iff a dictionary type occurs anywhere inside `dt`.
fn contains_dictionary(dt: &DataType) -> bool {
    match dt {
        DataType::Dictionary(_, _) => true,
        DataType::List(f) | DataType::LargeList(f) | DataType::FixedSizeList(f, _) | DataType::Map(f, _) => {
            contains_dictionary(f.data_type())
        }
        DataType::Struct(fs) => fs.iter().any(|f| contains_dictionary(f.data_type())),
        DataType::Union(fs, _) => fs.iter().any(|(_, f)| contains_dictionary(f.data_type())),
        DataType::RunEndEncoded(_, v) => contains_dictionary(v.data_type()),
        _ => false,
    }
}

/// Dictionary encoding is a producer-side compression, not a morloc type:
/// a dictionary column is decoded to its value type so `Str` always
/// travels as plain UTF-8 and every consumer sees the same physical form.
/// A dictionary nested inside another type is refused by column name.
fn decode_dictionaries(batch: &RecordBatch) -> Result<RecordBatch, MorlocError> {
    let mut changed = false;
    let mut fields: Vec<Field> = Vec::with_capacity(batch.num_columns());
    let mut columns: Vec<ArrayRef> = Vec::with_capacity(batch.num_columns());
    for (field, col) in batch.schema().fields().iter().zip(batch.columns()) {
        match field.data_type() {
            DataType::Dictionary(_, value) => {
                if contains_dictionary(value) {
                    return Err(err(format!(
                        "column '{}' has a dictionary nested inside {}, which is not supported",
                        field.name(),
                        value
                    )));
                }
                let plain = arrow_cast::cast(col, value.as_ref())
                    .map_err(|e| arrow_err(&format!("decoding dictionary column '{}'", field.name()), e))?;
                fields.push(field.as_ref().clone().with_data_type(value.as_ref().clone()));
                columns.push(plain);
                changed = true;
            }
            other if contains_dictionary(other) => {
                return Err(err(format!(
                    "column '{}' has a dictionary nested inside {}, which is not supported",
                    field.name(),
                    other
                )));
            }
            _ => {
                fields.push(field.as_ref().clone());
                columns.push(col.clone());
            }
        }
    }
    if !changed {
        return Ok(batch.clone());
    }
    let schema = ArrowSchema::new_with_metadata(fields, batch.schema().metadata().clone());
    RecordBatch::try_new(Arc::new(schema), columns).map_err(|e| arrow_err("rebuilding batch", e))
}

/// The Arrow type a declared morloc column type stands for, or None when
/// the declared type has no Arrow form. A `Str` is accepted in any text
/// form (see `declared_accepts`) and anything else is rendered as `Utf8`.
fn declared_target(st: SerialType) -> Option<DataType> {
    Some(match st {
        SerialType::String => DataType::Utf8,
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
        _ => return None,
    })
}

/// True iff an Arrow type is an acceptable physical form of a declared
/// morloc column type.
fn declared_accepts(st: SerialType, dt: &DataType) -> bool {
    match st {
        SerialType::String => matches!(dt, DataType::Utf8 | DataType::LargeUtf8 | DataType::Utf8View),
        other => declared_target(other).map_or(false, |t| &t == dt),
    }
}

/// Split a declared column type into (inner type, is optional).
fn declared_inner(p: &Schema) -> (SerialType, bool) {
    if p.serial_type == SerialType::Optional {
        (
            p.parameters.first().map(|c| c.serial_type).unwrap_or(SerialType::Nil),
            true,
        )
    } else {
        (p.serial_type, false)
    }
}

/// Bring a batch into agreement with the declared morloc column schema.
/// Declared columns are a lower bound on the batch (open semantics): each
/// must be present; extra columns pass through. The result carries the
/// declared columns first, in declared order, then the extras in their
/// original order. A declared column whose physical type differs is cast,
/// failing on any lossy conversion. A column declared non-optional must
/// hold no nulls.
pub fn align_to_declared(batch: &RecordBatch, declared: &Schema) -> Result<RecordBatch, MorlocError> {
    if declared.serial_type != SerialType::Table {
        return Err(err("align_to_declared: schema is not a Table"));
    }
    if declared.size == 0 {
        return Ok(batch.clone());
    }
    let schema = batch.schema();
    let mut fields: Vec<Field> = schema.fields().iter().map(|f| f.as_ref().clone()).collect();
    let mut columns: Vec<ArrayRef> = batch.columns().to_vec();
    let mut changed = false;
    let mut order: Vec<usize> = Vec::with_capacity(fields.len());

    for (k, p) in declared.keys.iter().zip(declared.parameters.iter()) {
        let idx = schema
            .index_of(k)
            .map_err(|_| err(format!("declared column '{}' missing from the table", k)))?;
        order.push(idx);
        let (inner, optional) = declared_inner(p);
        let col = &columns[idx];

        if !declared_accepts(inner, col.data_type()) {
            let target = declared_target(inner).ok_or_else(|| {
                err(format!(
                    "column '{}' has Arrow type {} but is declared as {:?}, which has no Arrow form",
                    k,
                    col.data_type(),
                    inner
                ))
            })?;
            let opts = arrow_cast::CastOptions { safe: false, ..Default::default() };
            let cast = arrow_cast::cast_with_options(col, &target, &opts).map_err(|e| {
                err(format!(
                    "column '{}' has Arrow type {} and cannot be converted to the declared {:?}: {}",
                    k,
                    col.data_type(),
                    inner,
                    e
                ))
            })?;
            // A conversion is accepted only when it loses nothing: casting
            // back must reproduce the original exactly (1.5 -> 1 -> 1.0 is
            // refused, "7" -> 7 -> "7" and int32 -> int64 are fine).
            let back = arrow_cast::cast_with_options(&cast, col.data_type(), &opts).ok();
            let lossless = back.as_ref().map_or(false, |b| b.as_ref() == col.as_ref());
            if !lossless {
                return Err(err(format!(
                    "column '{}' has Arrow type {} whose values do not convert losslessly to the declared {:?}",
                    k,
                    col.data_type(),
                    inner
                )));
            }
            fields[idx] = fields[idx].clone().with_data_type(target);
            columns[idx] = cast;
            changed = true;
        }

        // The block records the exact null count, so that is the check; the
        // producer's nullable flag is left as its library set it, which
        // keeps the schema equal to what that library would build itself.
        if !optional {
            let nulls = columns[idx].null_count();
            if nulls > 0 {
                return Err(err(format!(
                    "column '{}' is declared non-nullable ({:?}) but contains {} null(s); declare it optional",
                    k, inner, nulls
                )));
            }
        }
    }

    for i in 0..fields.len() {
        if !order.contains(&i) {
            order.push(i);
        }
    }
    let reordered = order.iter().enumerate().any(|(pos, &i)| pos != i);
    if !changed && !reordered {
        return Ok(batch.clone());
    }
    let fields: Vec<Field> = order.iter().map(|&i| fields[i].clone()).collect();
    let columns: Vec<ArrayRef> = order.iter().map(|&i| columns[i].clone()).collect();
    let new_schema = ArrowSchema::new_with_metadata(fields, schema.metadata().clone());
    RecordBatch::try_new(Arc::new(new_schema), columns).map_err(|e| arrow_err("rebuilding batch", e))
}

/// True iff any node in the tree is a slice (nonzero offset) or carries a
/// validity bitmap whose bit offset disagrees with the data offset.
fn needs_compaction(data: &ArrayData) -> bool {
    if data.offset() != 0 {
        return true;
    }
    if let Some(n) = data.nulls() {
        if n.offset() != 0 {
            return true;
        }
    }
    data.child_data().iter().any(needs_compaction)
}

/// Rebuild a sliced array as a tight, zero-offset copy so that every
/// buffer holds exactly the bytes of the rows it describes.
fn compact(data: &ArrayData) -> ArrayData {
    let mut m = MutableArrayData::new(vec![data], false, data.len());
    m.extend(0, 0, data.len());
    m.freeze()
}

/// Decode dictionaries and drop slices so the batch is ready to lay out.
pub fn normalize(batch: &RecordBatch) -> Result<RecordBatch, MorlocError> {
    let batch = decode_dictionaries(batch)?;
    let root = StructArray::from(batch.clone()).into_data();
    if !needs_compaction(&root) {
        return Ok(batch);
    }
    let tight = StructArray::from(compact(&root));
    RecordBatch::try_new(batch.schema(), tight.columns().to_vec())
        .map_err(|e| arrow_err("compacting batch", e))
}

// -- Writer ------------------------------------------------------------------

/// Byte length of a C Data Interface metadata blob: an int32 pair count
/// followed by (int32 length, bytes) for each key and each value. For a
/// blob handed over by a producer's library, which owns its memory.
unsafe fn c_metadata_len(p: *const c_char) -> usize {
    if p.is_null() {
        return 0;
    }
    c_metadata_len_bounded(p as *const u8, usize::MAX).unwrap_or(0)
}

/// As `c_metadata_len` for a blob that must lie within `limit` bytes of
/// `p`: None when a count or length prefix would carry the walk past it.
unsafe fn c_metadata_len_bounded(p: *const u8, limit: usize) -> Option<usize> {
    let read_i32 = |at: usize| -> Option<i32> {
        if at.checked_add(4)? <= limit {
            Some(ptr::read_unaligned(p.add(at) as *const i32))
        } else {
            None
        }
    };
    let n = read_i32(0)?;
    if n < 0 {
        return None;
    }
    let mut cur = 4usize;
    for _ in 0..n {
        for _ in 0..2 {
            let len = read_i32(cur)?;
            if len < 0 {
                return None;
            }
            cur = cur.checked_add(4 + len as usize)?;
            if cur > limit {
                return None;
            }
        }
    }
    Some(cur)
}

struct BufPlan {
    kind: u32,
    src: *const u8,
    len: usize,
}

struct NodePlan {
    length: i64,
    null_count: i64,
    flags: i64,
    format: Vec<u8>,
    name: Option<Vec<u8>>,
    metadata: Option<Vec<u8>>,
    first_buffer: usize,
    n_buffers: u32,
    child_index: u32,
    n_children: u32,
}

/// Lay out a record batch as a fresh SHM block and return its relative
/// pointer. The batch must already be normalised.
pub fn batch_to_shm(batch: &RecordBatch) -> Result<RelPtr, MorlocError> {
    if batch.num_columns() == 0 {
        return Err(err("Arrow table must have at least one column"));
    }
    let ffi_schema = FFI_ArrowSchema::try_from(batch.schema().as_ref())
        .map_err(|e| arrow_err("exporting schema", e))?;
    let root = StructArray::from(batch.clone()).into_data();
    if needs_compaction(&root) {
        return Err(err("batch_to_shm: batch is not normalised"));
    }

    // Level-order walk so each node's children are contiguous. Variadic
    // (view) layouts need one extra buffer of buffer lengths, which must
    // outlive the copy; `extra` keeps those alive.
    let mut nodes: Vec<NodePlan> = Vec::new();
    let mut bufs: Vec<BufPlan> = Vec::new();
    let mut extra: Vec<Vec<i64>> = Vec::new();
    let mut queue: std::collections::VecDeque<(*const FFI_ArrowSchema, *const ArrayData)> =
        std::collections::VecDeque::new();
    queue.push_back((&ffi_schema as *const _, &root as *const _));
    let mut next_index: u32 = 1;

    while let Some((s_ptr, d_ptr)) = queue.pop_front() {
        // SAFETY: both trees outlive the walk; pointers were taken from live nodes.
        let (s, d) = unsafe { (&*s_ptr, &*d_ptr) };
        if s.dictionary().is_some() {
            return Err(err("dictionary arrays must be decoded before layout"));
        }
        let n_children = d.child_data().len();
        if n_children != s.children().count() {
            return Err(err("schema and array child counts disagree"));
        }

        let lay = layout(d.data_type());
        let first_buffer = bufs.len();
        if lay.can_contain_null_mask {
            match d.nulls() {
                Some(n) => bufs.push(BufPlan {
                    kind: BUF_LOCAL,
                    src: n.buffer().as_ptr(),
                    len: n.buffer().len(),
                }),
                None => bufs.push(BufPlan { kind: BUF_NULL, src: ptr::null(), len: 0 }),
            }
        }
        for b in d.buffers() {
            bufs.push(BufPlan { kind: BUF_LOCAL, src: b.as_ptr(), len: b.len() });
        }
        if lay.variadic {
            let lens: Vec<i64> = d.buffers().iter().skip(1).map(|b| b.len() as i64).collect();
            extra.push(lens);
            let v = extra.last().unwrap();
            bufs.push(BufPlan {
                kind: BUF_LOCAL,
                src: v.as_ptr() as *const u8,
                len: v.len() * 8,
            });
        }

        let child_index = if n_children > 0 { next_index } else { 0 };
        next_index += n_children as u32;

        // The raw C fields are read through the layout-identical mirror:
        // the flags word travels verbatim and the metadata blob is copied
        // as bytes, neither of which the typed accessors expose.
        let raw = unsafe { &*(s as *const FFI_ArrowSchema as *const RawSchema) };
        nodes.push(NodePlan {
            length: d.len() as i64,
            null_count: match d.data_type() {
                DataType::Null => d.len() as i64,
                _ => d.null_count() as i64,
            },
            flags: raw.flags,
            format: s.format().as_bytes().to_vec(),
            name: s.name().filter(|n| !n.is_empty()).map(|n| n.as_bytes().to_vec()),
            metadata: unsafe {
                if raw.metadata.is_null() {
                    None
                } else {
                    let len = c_metadata_len(raw.metadata);
                    Some(std::slice::from_raw_parts(raw.metadata as *const u8, len).to_vec())
                }
            },
            first_buffer,
            n_buffers: (bufs.len() - first_buffer) as u32,
            child_index,
            n_children: n_children as u32,
        });

        for (cs, cd) in s.children().zip(d.child_data().iter()) {
            queue.push_back((cs as *const _, cd as *const _));
        }
    }

    // String table: NUL at 0, then each string NUL-terminated. Metadata
    // blobs are self-delimiting but get a trailing NUL too, harmlessly.
    let mut strtab: Vec<u8> = vec![0];
    let mut push_str = |bytes: &[u8]| -> u32 {
        let off = strtab.len() as u32;
        strtab.extend_from_slice(bytes);
        strtab.push(0);
        off
    };
    let mut str_offsets: Vec<(u32, u32, u32)> = Vec::with_capacity(nodes.len());
    for n in &nodes {
        let f = push_str(&n.format);
        let nm = n.name.as_ref().map_or(0, |b| push_str(b));
        let md = n.metadata.as_ref().map_or(0, |b| push_str(b));
        str_offsets.push((f, nm, md));
    }

    let header_size = std::mem::size_of::<ArrowShmHeader>();
    let nodes_offset = header_size;
    let buffers_offset = nodes_offset + nodes.len() * std::mem::size_of::<ArrowNodeDesc>();
    let strtab_offset = buffers_offset + bufs.len() * std::mem::size_of::<ArrowBufferDesc>();
    let data_start = align_up(strtab_offset + strtab.len());

    let mut buf_offsets: Vec<usize> = Vec::with_capacity(bufs.len());
    let mut cursor = data_start;
    for b in &bufs {
        cursor = align_up(cursor);
        buf_offsets.push(cursor);
        if b.kind == BUF_LOCAL {
            cursor += b.len;
        }
    }
    let total_size = cursor;

    let base = shm::shmalloc(total_size)?;
    // SAFETY: `base` addresses `total_size` writable bytes; every write
    // below stays inside that range by construction of the offsets.
    unsafe {
        ptr::write(
            base as *mut ArrowShmHeader,
            ArrowShmHeader {
                magic: ARROW_SHM_MAGIC,
                version: ARROW_SHM_VERSION,
                n_columns: batch.num_columns() as u32,
                n_nodes: nodes.len() as u32,
                n_rows: batch.num_rows() as u64,
                total_size: total_size as u64,
                metadata_offset: 0,
                metadata_length: 0,
                n_buffers: bufs.len() as u32,
                nodes_offset: nodes_offset as u32,
                buffers_offset: buffers_offset as u32,
                strtab_offset: strtab_offset as u32,
                strtab_length: strtab.len() as u32,
            },
        );
        let node_descs = base.add(nodes_offset) as *mut ArrowNodeDesc;
        for (i, n) in nodes.iter().enumerate() {
            let (f, nm, md) = str_offsets[i];
            ptr::write(
                node_descs.add(i),
                ArrowNodeDesc {
                    length: n.length,
                    null_count: n.null_count,
                    offset: 0,
                    flags: n.flags,
                    format_offset: f,
                    name_offset: nm,
                    metadata_offset: md,
                    buffer_index: n.first_buffer as u32,
                    n_buffers: n.n_buffers,
                    child_index: n.child_index,
                    n_children: n.n_children,
                    dictionary_index: 0,
                },
            );
        }
        let buf_descs = base.add(buffers_offset) as *mut ArrowBufferDesc;
        for (i, b) in bufs.iter().enumerate() {
            ptr::write(
                buf_descs.add(i),
                ArrowBufferDesc {
                    size: b.len as u64,
                    offset: buf_offsets[i] as u64,
                    extern_ref: 0,
                    kind: b.kind,
                    _pad: 0,
                },
            );
        }
        ptr::copy_nonoverlapping(strtab.as_ptr(), base.add(strtab_offset), strtab.len());
        // Only the padding is zeroed: the block's bytes are a cache key, so
        // the gaps between buffers must be deterministic.
        let mut end = strtab_offset + strtab.len();
        for (i, b) in bufs.iter().enumerate() {
            let off = buf_offsets[i];
            if off > end {
                ptr::write_bytes(base.add(end), 0, off - end);
            }
            if b.kind == BUF_LOCAL && b.len > 0 {
                ptr::copy_nonoverlapping(b.src, base.add(off), b.len);
            }
            end = off + if b.kind == BUF_LOCAL { b.len } else { 0 };
        }
        if total_size > end {
            ptr::write_bytes(base.add(end), 0, total_size - end);
        }
    }
    COPIED_BYTES.fetch_add(bufs.iter().map(|b| b.len as u64).sum(), std::sync::atomic::Ordering::Relaxed);

    match shm::abs2rel(base) {
        Ok(r) => Ok(r),
        Err(e) => {
            let _ = shm::shfree(base);
            Err(e)
        }
    }
}

/// Bytes memcpy'd into SHM by table writes in this process.
pub static COPIED_BYTES: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

/// The full producer pipeline: align to the declared column schema (if
/// any), normalise, lay out.
pub fn write_batch(batch: &RecordBatch, declared: Option<&Schema>) -> Result<RelPtr, MorlocError> {
    let aligned = match declared {
        Some(d) => align_to_declared(batch, d)?,
        None => batch.clone(),
    };
    let tight = normalize(&aligned)?;
    batch_to_shm(&tight)
}

/// Adopt caller-owned C Data Interface structs as a record batch. Takes
/// ownership of `array`; the caller must not release it afterwards. The
/// schema is only read.
///
/// # Safety
/// Both pointers must address valid, unreleased structs.
pub unsafe fn ffi_to_batch(
    array: *mut FFI_ArrowArray,
    schema: *const FFI_ArrowSchema,
) -> Result<RecordBatch, MorlocError> {
    if array.is_null() || schema.is_null() {
        return Err(err("NULL array or schema"));
    }
    let owned = FFI_ArrowArray::from_raw(array);
    let data = from_ffi(owned, &*schema).map_err(|e| arrow_err("importing array", e))?;
    match data.data_type() {
        DataType::Struct(_) => {}
        other => return Err(err(format!("expected a struct array for a table, found {}", other))),
    }
    if data.null_count() > 0 {
        return Err(err("a table's rows cannot themselves be null (the struct array carries a validity bitmap)"));
    }
    Ok(RecordBatch::from(StructArray::from(data)))
}

// -- Reader ------------------------------------------------------------------

/// Checked view over a block: every offset and range validated once.
pub struct BlockView {
    base: *const u8,
    header: *const ArrowShmHeader,
    nodes: *const ArrowNodeDesc,
    bufs: *const ArrowBufferDesc,
    strtab: *const u8,
}

impl BlockView {
    /// # Safety
    /// `header` must point at readable memory of at least the length the
    /// header claims.
    pub unsafe fn open(header: *const ArrowShmHeader) -> Result<BlockView, MorlocError> {
        if header.is_null() {
            return Err(err("NULL arrow header"));
        }
        let h = &*header;
        if h.magic != ARROW_SHM_MAGIC {
            return Err(err(format!("Invalid arrow SHM magic: 0x{:08x}", h.magic)));
        }
        if h.version != ARROW_SHM_VERSION {
            return Err(err(format!(
                "Arrow SHM block has wire version {}, this runtime speaks {}; rebuild with \
                 'MORLOC_RUST_DIR=$PWD/data/rust morloc init -f'",
                h.version, ARROW_SHM_VERSION
            )));
        }
        let total = h.total_size as usize;
        let fits = |off: usize, len: usize| off <= total && len <= total - off;
        if h.n_nodes == 0
            || !fits(h.nodes_offset as usize, h.n_nodes as usize * 64)
            || !fits(h.buffers_offset as usize, h.n_buffers as usize * 32)
            || !fits(h.strtab_offset as usize, h.strtab_length as usize)
            || h.strtab_length == 0
            || (h.metadata_length > 0 && !fits(h.metadata_offset as usize, h.metadata_length as usize))
        {
            return Err(err("Arrow SHM header describes ranges outside the block"));
        }
        let base = header as *const u8;
        let v = BlockView {
            base,
            header,
            nodes: base.add(h.nodes_offset as usize) as *const ArrowNodeDesc,
            bufs: base.add(h.buffers_offset as usize) as *const ArrowBufferDesc,
            strtab: base.add(h.strtab_offset as usize),
        };
        v.validate_structure()?;
        Ok(v)
    }

    unsafe fn validate_structure(&self) -> Result<(), MorlocError> {
        let h = &*self.header;
        let n_nodes = h.n_nodes as usize;
        let n_bufs = h.n_buffers as usize;
        let strlen = h.strtab_length as usize;
        let total = h.total_size as usize;
        if *self.strtab != 0 || *self.strtab.add(strlen - 1) != 0 {
            return Err(err("Arrow SHM string table is not NUL-framed"));
        }
        for k in 0..n_nodes {
            let n = &*self.nodes.add(k);
            let str_ok = |off: u32| (off as usize) < strlen;
            if n.format_offset == 0 || !str_ok(n.format_offset) || !str_ok(n.name_offset) || !str_ok(n.metadata_offset) {
                return Err(err(format!("Arrow SHM node {} names a string outside the table", k)));
            }
            if n.metadata_offset != 0
                && c_metadata_len_bounded(self.strtab.add(n.metadata_offset as usize), strlen - n.metadata_offset as usize)
                    .is_none()
            {
                return Err(err(format!("Arrow SHM node {} has a metadata blob that leaves the string table", k)));
            }
            if n.buffer_index as usize + n.n_buffers as usize > n_bufs {
                return Err(err(format!("Arrow SHM node {} names buffers outside the table", k)));
            }
            if n.n_children > 0
                && ((n.child_index as usize) <= k || n.child_index as usize + n.n_children as usize > n_nodes)
            {
                return Err(err(format!("Arrow SHM node {} has an invalid child range", k)));
            }
            if n.dictionary_index != 0 {
                return Err(err(format!("Arrow SHM node {} carries a dictionary, which is not supported", k)));
            }
            if n.length < 0 || n.null_count < -1 || n.offset != 0 {
                return Err(err(format!("Arrow SHM node {} has invalid length/null_count/offset", k)));
            }
        }
        if (*self.nodes).n_children != h.n_columns {
            return Err(err("Arrow SHM root child count disagrees with n_columns"));
        }
        for i in 0..n_bufs {
            let b = &*self.bufs.add(i);
            match b.kind {
                BUF_NULL => {}
                BUF_LOCAL => {
                    let off = b.offset as usize;
                    let sz = b.size as usize;
                    if off > total || sz > total - off {
                        return Err(err(format!("Arrow SHM buffer {} lies outside the block", i)));
                    }
                }
                _ => return Err(err(format!("Arrow SHM buffer {} has unsupported kind {}", i, b.kind))),
            }
        }
        self.validate_lengths()
    }

    /// The C Data Interface schema of the subtree rooted at node `k`,
    /// built from the format strings so arrow-rs can name each node's
    /// type. Only the types matter here, not names or metadata.
    unsafe fn node_type(&self, k: usize, depth: usize) -> Result<FFI_ArrowSchema, MorlocError> {
        if depth > 64 {
            return Err(err("Arrow SHM node tree is nested more than 64 deep"));
        }
        let n = self.node(k);
        let children = (0..n.n_children as usize)
            .map(|j| self.node_type(n.child_index as usize + j, depth + 1))
            .collect::<Result<Vec<_>, _>>()?;
        let format = self.string_str(n.format_offset);
        FFI_ArrowSchema::try_new(format, children, None)
            .map_err(|e| err(format!("Arrow SHM node {} has format '{}' this runtime cannot read: {}", k, format, e)))
    }

    /// Every node's length must be covered by the buffers it names and by
    /// its children, since consumers derive buffer lengths from `length`
    /// and would otherwise read past the block.
    unsafe fn validate_lengths(&self) -> Result<(), MorlocError> {
        let root = DataType::try_from(&self.node_type(0, 0)?)
            .map_err(|e| err(format!("Arrow SHM root has a type this runtime cannot read: {}", e)))?;
        let h = self.header();
        if self.node(0).length as u64 != h.n_rows {
            return Err(err("Arrow SHM root length disagrees with the header's row count"));
        }
        let mut queue: std::collections::VecDeque<(usize, DataType)> = std::collections::VecDeque::new();
        queue.push_back((0, root));
        while let Some((k, dt)) = queue.pop_front() {
            let n = self.node(k);
            self.validate_node_buffers(k, &dt)?;
            let children = child_types(&dt);
            if children.len() != n.n_children as usize {
                return Err(err(format!(
                    "Arrow SHM node {} has {} children but its type {} needs {}",
                    k, n.n_children, dt, children.len()
                )));
            }
            for (j, c) in children.into_iter().enumerate() {
                let ck = n.child_index as usize + j;
                let cl = self.node(ck).length;
                let need = match &dt {
                    DataType::Struct(_) | DataType::Union(_, arrow_schema::UnionMode::Sparse) => Some(n.length),
                    DataType::FixedSizeList(_, w) => Some(n.length.saturating_mul(*w as i64)),
                    _ => self.last_offset_of(n, &dt)?,
                };
                if let Some(need) = need {
                    if cl < need {
                        return Err(err(format!(
                            "Arrow SHM node {} has length {} but its parent node {} needs at least {}",
                            ck, cl, k, need
                        )));
                    }
                }
                queue.push_back((ck, c));
            }
        }
        Ok(())
    }

    /// The width of the offsets an Arrow type carries in its first buffer,
    /// or None for a type without offsets.
    fn offset_width(dt: &DataType) -> Option<usize> {
        match dt {
            DataType::Utf8 | DataType::Binary | DataType::List(_) | DataType::Map(_, _) => Some(4),
            DataType::LargeUtf8 | DataType::LargeBinary | DataType::LargeList(_) => Some(8),
            _ => None,
        }
    }

    /// The last entry of a node's offsets buffer, which bounds its data
    /// buffer or child; None for a type without offsets.
    unsafe fn last_offset_of(&self, n: &ArrowNodeDesc, dt: &DataType) -> Result<Option<i64>, MorlocError> {
        Ok(match Self::offset_width(dt) {
            Some(4) => Some(self.last_offset::<i32>(n)?),
            Some(_) => Some(self.last_offset::<i64>(n)?),
            None => None,
        })
    }

    /// The last entry of a node's offsets buffer (the buffer after the
    /// validity slot).
    unsafe fn last_offset<T: arrow_buffer::ArrowNativeType + Into<i64>>(&self, n: &ArrowNodeDesc) -> Result<i64, MorlocError> {
        if n.length == 0 {
            return Ok(0);
        }
        let b = self.buffer(n.buffer_index as usize + 1);
        let at = n.length as usize * std::mem::size_of::<T>();
        if b.kind != BUF_LOCAL || (b.size as usize) < at + std::mem::size_of::<T>() {
            return Err(err("Arrow SHM offsets buffer is shorter than the node's length"));
        }
        let v: T = ptr::read_unaligned(self.base.add(b.offset as usize + at) as *const T);
        Ok(v.into())
    }

    unsafe fn validate_node_buffers(&self, k: usize, dt: &DataType) -> Result<(), MorlocError> {
        let n = self.node(k);
        let length = n.length as usize;
        let lay = layout(dt);
        let bad = |what: String| err(format!("Arrow SHM node {} ({}): {}", k, dt, what));
        let size_of = |i: usize| -> usize {
            let b = self.buffer(n.buffer_index as usize + i);
            if b.kind == BUF_LOCAL { b.size as usize } else { 0 }
        };
        let bitmap_bytes = (length + 7) / 8;

        let mut i = 0usize;
        let mut expected = lay.buffers.len();
        if lay.can_contain_null_mask {
            expected += 1;
            if (n.n_buffers as usize) < 1 {
                return Err(bad("no validity slot".into()));
            }
            let b = self.buffer(n.buffer_index as usize);
            if b.kind == BUF_NULL {
                if n.null_count > 0 {
                    return Err(bad(format!("null_count {} with no validity bitmap", n.null_count)));
                }
            } else if size_of(0) < bitmap_bytes {
                return Err(bad(format!("validity bitmap of {} bytes for length {}", size_of(0), length)));
            }
            i = 1;
        }
        if lay.variadic {
            // Views: the views buffer, N data buffers, then N recorded sizes.
            if (n.n_buffers as usize) < expected + 1 {
                return Err(bad("too few buffers for a view type".into()));
            }
            let n_data = n.n_buffers as usize - expected - 1;
            if size_of(i) < length * 16 {
                return Err(bad(format!("views buffer of {} bytes for length {}", size_of(i), length)));
            }
            let sizes = self.buffer(n.buffer_index as usize + n.n_buffers as usize - 1);
            if sizes.kind != BUF_LOCAL || (sizes.size as usize) < n_data * 8 {
                return Err(bad("buffer sizes table shorter than the data buffer count".into()));
            }
            for d in 0..n_data {
                let recorded = ptr::read_unaligned(self.base.add(sizes.offset as usize + d * 8) as *const i64);
                if recorded < 0 || recorded as usize > size_of(i + 1 + d) {
                    return Err(bad(format!("data buffer {} is shorter than its recorded size {}", d, recorded)));
                }
            }
            return Ok(());
        }
        if n.n_buffers as usize != expected {
            return Err(bad(format!("{} buffers where its type needs {}", n.n_buffers, expected)));
        }
        // Offsets-bearing types carry length + 1 entries in their first
        // buffer, and the buffer or child after it must reach the last one.
        let has_offsets = Self::offset_width(dt).is_some();
        for (j, spec) in lay.buffers.iter().enumerate() {
            let have = size_of(i + j);
            let need = match spec {
                arrow_data::BufferSpec::FixedWidth { byte_width, .. } => {
                    if j == 0 && has_offsets && length > 0 {
                        (length + 1) * byte_width
                    } else {
                        length * byte_width
                    }
                }
                arrow_data::BufferSpec::BitMap => bitmap_bytes,
                arrow_data::BufferSpec::VariableWidth => {
                    let last = self.last_offset_of(n, dt)?.unwrap_or(0);
                    if last < 0 {
                        return Err(bad("negative last offset".into()));
                    }
                    last as usize
                }
                arrow_data::BufferSpec::AlwaysNull => 0,
            };
            if have < need {
                return Err(bad(format!("buffer {} holds {} bytes but length {} needs {}", i + j, have, length, need)));
            }
        }
        Ok(())
    }

    pub fn header(&self) -> &ArrowShmHeader {
        // SAFETY: validated in `open`.
        unsafe { &*self.header }
    }

    pub fn node(&self, k: usize) -> &ArrowNodeDesc {
        // SAFETY: k < n_nodes is the caller's obligation; ranges validated in `open`.
        unsafe { &*self.nodes.add(k) }
    }

    pub fn buffer(&self, i: usize) -> &ArrowBufferDesc {
        unsafe { &*self.bufs.add(i) }
    }

    /// C string at a string-table offset; offset 0 is the empty string.
    pub fn string(&self, off: u32) -> *const c_char {
        unsafe { self.strtab.add(off as usize) as *const c_char }
    }

    pub fn string_str(&self, off: u32) -> &str {
        unsafe { CStr::from_ptr(self.string(off)).to_str().unwrap_or("") }
    }

    pub fn buffer_ptr(&self, i: usize) -> *const c_void {
        let b = self.buffer(i);
        if b.kind == BUF_LOCAL {
            unsafe { self.base.add(b.offset as usize) as *const c_void }
        } else {
            ptr::null()
        }
    }

    /// Index of the root's child named `name`, if any.
    pub fn column_index(&self, name: &str) -> Option<usize> {
        let root = self.node(0);
        (0..root.n_children as usize).find(|&j| {
            self.string_str(self.node(root.child_index as usize + j).name_offset) == name
        })
    }

    pub fn column_node(&self, j: usize) -> &ArrowNodeDesc {
        self.node(self.node(0).child_index as usize + j)
    }
}

/// The child types of a nested Arrow type, in child order.
fn child_types(dt: &DataType) -> Vec<DataType> {
    match dt {
        DataType::Struct(fields) => fields.iter().map(|f| f.data_type().clone()).collect(),
        DataType::List(f)
        | DataType::LargeList(f)
        | DataType::ListView(f)
        | DataType::LargeListView(f)
        | DataType::FixedSizeList(f, _)
        | DataType::Map(f, _) => vec![f.data_type().clone()],
        DataType::Union(fields, _) => fields.iter().map(|(_, f)| f.data_type().clone()).collect(),
        DataType::RunEndEncoded(r, v) => vec![r.data_type().clone(), v.data_type().clone()],
        _ => vec![],
    }
}

// -- Import: pointer fixup into one arena ------------------------------------

/// Arena owned by an imported view. Both root structs point at it; the
/// last root released frees it and, when the view owns the block it reads,
/// releases the block's reference too.
///
/// `live_roots` is atomic because the two roots are released wherever the
/// importing language frees its objects, which need not be the thread that
/// built the view. `owner_pid` is the process that took the reference: a
/// child that inherited a live view across a fork must not decrement a
/// count its parent still owns.
#[repr(C)]
struct ImportArena {
    magic: u32,
    live_roots: std::sync::atomic::AtomicU32,
    bytes: *mut u8,
    len: usize,
    block: *mut u8,
    block_bytes: usize,
    owner_pid: libc::pid_t,
}

const IMPORT_MAGIC: u32 = 0x4D4C4341; // "MLCA"

unsafe extern "C" fn release_child_schema(s: *mut FFI_ArrowSchema) {
    if !s.is_null() {
        (*(s as *mut RawSchema)).release = None;
    }
}

unsafe extern "C" fn release_child_array(a: *mut FFI_ArrowArray) {
    if !a.is_null() {
        (*(a as *mut RawArray)).release = None;
    }
}

unsafe fn release_arena(private_data: *mut c_void) {
    use std::sync::atomic::Ordering;
    let arena = private_data as *mut ImportArena;
    if arena.is_null() || (*arena).magic != IMPORT_MAGIC {
        return;
    }
    // A consumer that releases a struct it was told to consider consumed
    // would otherwise wrap the count and take the block from whoever still
    // holds it.
    if (*arena).live_roots.load(Ordering::Acquire) == 0 {
        return;
    }
    if (*arena).live_roots.fetch_sub(1, Ordering::AcqRel) != 1 {
        return;
    }
    let block = (*arena).block;
    // The entry must go before the block does: a candidate in the registry
    // is one some view still holds a reference on, which is what makes it
    // safe for `try_borrow` to read.
    if !block.is_null() {
        borrow_forget(arena);
        LIVE_VIEW_BYTES.fetch_sub((*arena).block_bytes, Ordering::Relaxed);
        if libc::getpid() == (*arena).owner_pid {
            let _ = shm::shfree(block);
        }
    }
    libc::free((*arena).bytes as *mut c_void);
}

/// Bytes of shared memory held by the views this process has open over
/// table blocks. A language that frees its objects on its own schedule
/// uses this to tell a backlog of unreachable views from ordinary work:
/// the count of views says nothing about what they cost, and a table's
/// cost is the whole point.
pub static LIVE_VIEW_BYTES: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

unsafe extern "C" fn release_root_schema(s: *mut FFI_ArrowSchema) {
    if s.is_null() {
        return;
    }
    let raw = s as *mut RawSchema;
    let pd = (*raw).private_data;
    (*raw).release = None;
    release_arena(pd);
}

unsafe extern "C" fn release_root_array(a: *mut FFI_ArrowArray) {
    if a.is_null() {
        return;
    }
    let raw = a as *mut RawArray;
    let pd = (*raw).private_data;
    (*raw).release = None;
    release_arena(pd);
}

/// Field-level mirrors of the C structs, used to write the import
/// results. Layout-identical to the arrow-rs FFI types (checked below).
#[repr(C)]
pub struct RawSchema {
    format: *const c_char,
    name: *const c_char,
    metadata: *const c_char,
    flags: i64,
    n_children: i64,
    children: *mut *mut RawSchema,
    dictionary: *mut RawSchema,
    release: Option<unsafe extern "C" fn(*mut FFI_ArrowSchema)>,
    private_data: *mut c_void,
}

#[repr(C)]
pub struct RawArray {
    pub length: i64,
    pub null_count: i64,
    pub offset: i64,
    pub n_buffers: i64,
    pub n_children: i64,
    pub buffers: *mut *const c_void,
    pub children: *mut *mut RawArray,
    pub dictionary: *mut RawArray,
    pub release: Option<unsafe extern "C" fn(*mut FFI_ArrowArray)>,
    pub private_data: *mut c_void,
}

const _: () = assert!(std::mem::size_of::<RawSchema>() == std::mem::size_of::<FFI_ArrowSchema>());
const _: () = assert!(std::mem::size_of::<RawArray>() == std::mem::size_of::<FFI_ArrowArray>());

/// Build C Data Interface views over a block. Buffer pointers, format
/// strings and names point straight into the block; the only allocation
/// is one arena for the child structs, freed when both roots are
/// released. The block itself must outlive the view.
///
/// # Safety
/// `header` must be a live block; the out pointers must be writable.
pub unsafe fn shm_to_ffi(
    header: *const ArrowShmHeader,
    out_schema: *mut FFI_ArrowSchema,
    out_array: *mut FFI_ArrowArray,
) -> Result<(), MorlocError> {
    shm_to_ffi_inner(header, None, out_schema, out_array)
}

/// As `shm_to_ffi`, with the view owning one reference on the block, so
/// the block outlives every language object built from it and no longer
/// than that. `acquire` takes a reference of the view's own; otherwise the
/// view adopts the caller's, which is what a block this pool materialised
/// for itself wants. On failure nothing is taken and an adopted reference
/// is left with the caller.
///
/// # Safety
/// As `shm_to_ffi`, and the caller must hold a reference to adopt when
/// `acquire` is false.
pub unsafe fn shm_to_ffi_owned(
    header: *const ArrowShmHeader,
    acquire: bool,
    out_schema: *mut FFI_ArrowSchema,
    out_array: *mut FFI_ArrowArray,
) -> Result<(), MorlocError> {
    shm_to_ffi_inner(header, Some(acquire), out_schema, out_array)
}

/// `owns` is None for a view that only reads the block, Some(acquire) for
/// one that owns a reference on it.
unsafe fn shm_to_ffi_inner(
    header: *const ArrowShmHeader,
    owns: Option<bool>,
    out_schema: *mut FFI_ArrowSchema,
    out_array: *mut FFI_ArrowArray,
) -> Result<(), MorlocError> {
    // On failure the caller finds released (all-zero) structs, never
    // whatever its stack held before.
    ptr::write_bytes(out_schema as *mut RawSchema, 0, 1);
    ptr::write_bytes(out_array as *mut RawArray, 0, 1);
    let view = BlockView::open(header)?;
    let h = view.header();
    let n_nodes = h.n_nodes as usize;
    let n_bufs = h.n_buffers as usize;

    let sz_arena = std::mem::size_of::<ImportArena>();
    let sz_schema = std::mem::size_of::<RawSchema>();
    let sz_array = std::mem::size_of::<RawArray>();
    let off_schemas = align8(sz_arena);
    let off_arrays = align8(off_schemas + n_nodes * sz_schema);
    let off_schema_children = align8(off_arrays + n_nodes * sz_array);
    let off_array_children = align8(off_schema_children + n_nodes * 8);
    let off_buffers = align8(off_array_children + n_nodes * 8);
    let len = align8(off_buffers + n_bufs * 8);

    // The reference comes before the allocation that could fail, so a
    // failure has nothing to give back.
    let block = match owns {
        None => ptr::null_mut(),
        Some(true) => {
            // The sender donated a reference before the bytes left, so a
            // refusal means the block is already gone and the view would
            // read scrubbed memory.
            shm::shincref(header as *mut u8)
                .map_err(|e| err(format!("cannot hold the table's block: {}", e)))?;
            header as *mut u8
        }
        Some(false) => header as *mut u8,
    };

    let bytes = libc::calloc(1, len) as *mut u8;
    if bytes.is_null() {
        if let Some(true) = owns {
            let _ = shm::shfree(block);
        }
        return Err(err("out of memory importing arrow table"));
    }
    let arena = bytes as *mut ImportArena;
    ptr::write(
        arena,
        ImportArena {
            magic: IMPORT_MAGIC,
            live_roots: std::sync::atomic::AtomicU32::new(2),
            bytes,
            len,
            block,
            block_bytes: if block.is_null() { 0 } else { h.total_size as usize },
            owner_pid: libc::getpid(),
        },
    );
    if !block.is_null() {
        LIVE_VIEW_BYTES.fetch_add(h.total_size as usize, std::sync::atomic::Ordering::Relaxed);
        match shm::abs2rel(block) {
            Ok(rel) => borrow_register(block, rel, arena),
            // A block outside every mapped volume cannot be passed
            // through, but it can still be read.
            Err(_) => {}
        }
    }

    let schemas = bytes.add(off_schemas) as *mut RawSchema;
    let arrays = bytes.add(off_arrays) as *mut RawArray;
    let schema_children = bytes.add(off_schema_children) as *mut *mut RawSchema;
    let array_children = bytes.add(off_array_children) as *mut *mut RawArray;
    let buffers = bytes.add(off_buffers) as *mut *const c_void;

    for i in 0..n_bufs {
        *buffers.add(i) = view.buffer_ptr(i);
    }
    for k in 0..n_nodes {
        let n = view.node(k);
        let s = schemas.add(k);
        let a = arrays.add(k);
        (*s).format = view.string(n.format_offset);
        (*s).name = if n.name_offset == 0 { ptr::null() } else { view.string(n.name_offset) };
        (*s).metadata = if n.metadata_offset == 0 { ptr::null() } else { view.string(n.metadata_offset) };
        (*s).flags = n.flags;
        (*s).n_children = n.n_children as i64;
        (*s).children = if n.n_children > 0 { schema_children.add(n.child_index as usize) } else { ptr::null_mut() };
        (*s).dictionary = ptr::null_mut();
        (*s).release = Some(release_child_schema);
        (*s).private_data = ptr::null_mut();
        *schema_children.add(k) = s;

        (*a).length = n.length;
        (*a).null_count = n.null_count;
        (*a).offset = 0;
        (*a).n_buffers = n.n_buffers as i64;
        (*a).n_children = n.n_children as i64;
        (*a).buffers = buffers.add(n.buffer_index as usize);
        (*a).children = if n.n_children > 0 { array_children.add(n.child_index as usize) } else { ptr::null_mut() };
        (*a).dictionary = ptr::null_mut();
        (*a).release = Some(release_child_array);
        (*a).private_data = ptr::null_mut();
        *array_children.add(k) = a;
    }

    // Roots are copied out to the caller-owned structs; their release
    // callbacks own the arena.
    let root_s = ptr::read(schemas);
    let root_a = ptr::read(arrays);
    ptr::write(
        out_schema as *mut RawSchema,
        RawSchema { release: Some(release_root_schema), private_data: arena as *mut c_void, ..root_s },
    );
    ptr::write(
        out_array as *mut RawArray,
        RawArray { release: Some(release_root_array), private_data: arena as *mut c_void, ..root_a },
    );
    Ok(())
}

fn align8(x: usize) -> usize {
    (x + 7) & !7
}

/// The byte length of a checked block. A table's serialized form is the
/// block itself, so this is its size wherever a value's flat size is
/// needed (hashing, caching, inline packets).
///
/// # Safety
/// `header` must be a live block.
pub unsafe fn block_size(header: *const ArrowShmHeader) -> Result<usize, MorlocError> {
    let view = BlockView::open(header)?;
    Ok(view.header().total_size as usize)
}

/// Read a block back as an arrow-rs record batch. Buffers still point into
/// the block, so it must outlive the batch.
///
/// # Safety
/// `header` must be a live block.
pub unsafe fn shm_to_batch(header: *const ArrowShmHeader) -> Result<RecordBatch, MorlocError> {
    let mut s = FFI_ArrowSchema::empty();
    let mut a = FFI_ArrowArray::empty();
    shm_to_ffi(header, &mut s as *mut _, &mut a as *mut _)?;
    let data = from_ffi(a, &s).map_err(|e| arrow_err("importing table", e))?;
    Ok(RecordBatch::from(StructArray::from(data)))
}

/// The arrow-rs schema of a block without materialising its arrays.
///
/// # Safety
/// `header` must be a live block.
pub unsafe fn shm_schema(header: *const ArrowShmHeader) -> Result<SchemaRef, MorlocError> {
    let mut s = FFI_ArrowSchema::empty();
    let mut a = FFI_ArrowArray::empty();
    shm_to_ffi(header, &mut s as *mut _, &mut a as *mut _)?;
    let dt = DataType::try_from(&s).map_err(|e| arrow_err("importing schema", e))?;
    match dt {
        DataType::Struct(fields) => Ok(Arc::new(ArrowSchema::new(fields))),
        other => Err(err(format!("table root is {}, not a struct", other))),
    }
}

// -- Borrowing: returning a received table without copying it ----------------

/// A block some view in this process holds a reference on, and the arena
/// that holds it. A table returned unchanged is recognised against these
/// and passed through with a fresh reference instead of a copy.
///
/// The entry belongs to the view, not to the dispatch: a view may be
/// released on any thread, and a language object may outlive the dispatch
/// that built it. The invariant the readers depend on is that an entry
/// exists only while some view holds a reference, so a candidate is never
/// a block being scrubbed or rewritten under the reader.
struct BorrowEntry {
    base: *const u8,
    rel: RelPtr,
    arena: *const ImportArena,
}

// SAFETY: the pointers are only ever compared and dereferenced under the
// registry lock, and an entry is removed before its block is released.
unsafe impl Send for BorrowEntry {}

static BORROWABLE: std::sync::Mutex<Vec<BorrowEntry>> = std::sync::Mutex::new(Vec::new());

/// A poisoned registry is not a reason to abort: the lock is taken inside
/// release callbacks, which are `extern "C"` and cannot unwind.
fn borrowable() -> std::sync::MutexGuard<'static, Vec<BorrowEntry>> {
    BORROWABLE.lock().unwrap_or_else(|e| e.into_inner())
}

/// Record a block a new view holds a reference on.
fn borrow_register(base: *const u8, rel: RelPtr, arena: *const ImportArena) {
    borrowable().push(BorrowEntry { base, rel, arena });
}

/// Forget the entry of a view that is being released, before its block is.
fn borrow_forget(arena: *const ImportArena) {
    let mut reg = borrowable();
    if let Some(i) = reg.iter().position(|e| std::ptr::eq(e.arena, arena)) {
        reg.swap_remove(i);
    }
}

fn borrowing_disabled() -> bool {
    static FLAG: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *FLAG.get_or_init(|| std::env::var_os("MORLOC_ARROW_NO_BORROW").is_some())
}

/// True iff the C Data Interface tree rooted at (`s`, `a`) is exactly the
/// view `shm_to_ffi` builds from node `k` of `view`: same lengths, null
/// counts, flags, formats, names and buffer pointers, recursively. A
/// renamed column, a slice, or a recomputed buffer all fail the test, so
/// a false negative costs a copy and a false positive cannot happen.
unsafe fn same_as_block(view: &BlockView, k: usize, s: &RawSchema, a: &RawArray) -> bool {
    let n = view.node(k);
    if a.length != n.length
        || a.null_count != n.null_count
        || a.offset != 0
        || a.n_buffers != n.n_buffers as i64
        || a.n_children != n.n_children as i64
        || s.n_children != n.n_children as i64
        || s.flags != n.flags
        || !s.dictionary.is_null()
        || !a.dictionary.is_null()
    {
        return false;
    }
    if s.format.is_null() || libc::strcmp(s.format, view.string(n.format_offset)) != 0 {
        return false;
    }
    let name: &CStr = if s.name.is_null() { CStr::from_bytes_with_nul_unchecked(b"\0") } else { CStr::from_ptr(s.name) };
    if name.to_bytes() != CStr::from_ptr(view.string(n.name_offset)).to_bytes() {
        return false;
    }
    // Field metadata is part of identity too: an extension type lives
    // entirely in it, so a changed or dropped blob must force a copy.
    let block_meta: &[u8] = if n.metadata_offset == 0 {
        &[]
    } else {
        let p = view.string(n.metadata_offset);
        std::slice::from_raw_parts(p as *const u8, c_metadata_len(p))
    };
    let given_meta: &[u8] = if s.metadata.is_null() {
        &[]
    } else {
        std::slice::from_raw_parts(s.metadata as *const u8, c_metadata_len(s.metadata))
    };
    if block_meta != given_meta {
        return false;
    }
    for i in 0..n.n_buffers as usize {
        if *a.buffers.add(i) != view.buffer_ptr(n.buffer_index as usize + i) {
            return false;
        }
    }
    for j in 0..n.n_children as usize {
        let cs = *s.children.add(j);
        let ca = *a.children.add(j);
        if cs.is_null() || ca.is_null() || !same_as_block(view, n.child_index as usize + j, &*cs, &*ca) {
            return false;
        }
    }
    true
}

/// If the table is byte-for-byte a block this pool received, take a
/// reference on that block and return its relative pointer. The declared
/// schema, when given, must already hold for the block. Never consumes
/// the caller's structs.
///
/// # Safety
/// The pointers must address valid, unreleased structs.
pub unsafe fn try_borrow(
    array: *const FFI_ArrowArray,
    schema: *const FFI_ArrowSchema,
    declared: Option<&Schema>,
) -> Option<RelPtr> {
    if borrowing_disabled() || array.is_null() || schema.is_null() {
        return None;
    }
    let s = &*(schema as *const RawSchema);
    let a = &*(array as *const RawArray);
    // Held across the whole check: an entry that vanished mid-read would
    // leave the candidate free to be scrubbed or reallocated beneath it.
    let reg = borrowable();
    let trace = std::env::var_os("MORLOC_ARROW_STATS").is_some();
    if trace {
        eprintln!("try_borrow: {} candidate(s)", reg.len());
    }
    for &BorrowEntry { base, rel, .. } in reg.iter() {
        // The header alone rules out most candidates, before the block
        // is checked in full.
        let h = &*(base as *const ArrowShmHeader);
        if h.magic != ARROW_SHM_MAGIC || h.n_rows != a.length as u64 || h.n_columns as i64 != a.n_children {
            if trace { eprintln!("try_borrow: candidate differs in shape"); }
            continue;
        }
        let view = match BlockView::open(base as *const ArrowShmHeader) {
            Ok(v) => v,
            Err(e) => {
                if trace { eprintln!("try_borrow: candidate unreadable: {}", e); }
                continue;
            }
        };
        if !same_as_block(&view, 0, s, a) {
            if trace { eprintln!("try_borrow: candidate differs from the returned table"); }
            continue;
        }
        // The copy path would cast and reorder to the declared schema; a
        // block is passed through only when that would change nothing.
        if let Some(d) = declared {
            if d.size > 0 {
                if validate_view(&view, d).is_err() {
                    continue;
                }
                let in_order = d.keys.iter().enumerate().all(|(i, k)| view.column_index(k) == Some(i));
                if !in_order {
                    continue;
                }
            }
        }
        if shm::shincref(base as *mut u8).is_ok() {
            return Some(rel);
        }
    }
    None
}

// -- Validation against a declared morloc schema -----------------------------

/// Open-semantics check of a block against the morloc column schema it is
/// received under: every declared column is present with an acceptable
/// physical type, and a column declared non-optional holds no null.
///
/// # Safety
/// `header` must be a live block.
pub unsafe fn validate(header: *const ArrowShmHeader, declared: &Schema) -> Result<(), MorlocError> {
    validate_view(&BlockView::open(header)?, declared)
}

/// `validate` over an already checked view.
fn validate_view(view: &BlockView, declared: &Schema) -> Result<(), MorlocError> {
    if declared.serial_type != SerialType::Table {
        return Err(err("Expected a Table schema for arrow validation"));
    }
    for (k, p) in declared.keys.iter().zip(declared.parameters.iter()) {
        let j = view
            .column_index(k)
            .ok_or_else(|| err(format!("Declared column '{}' missing from arrow table", k)))?;
        let node = view.column_node(j);
        let format = view.string_str(node.format_offset);
        let (inner, optional) = declared_inner(p);
        let dt = format_to_datatype(format);
        let ok = match dt {
            Some(ref d) => declared_accepts(inner, d),
            None => false,
        };
        if !ok {
            return Err(err(format!(
                "Column '{}' has Arrow type '{}' but is declared as {:?}",
                k, format, inner
            )));
        }
        if !optional {
            if node.null_count > 0 {
                return Err(err(format!(
                    "Column '{}' is declared non-nullable ({:?}) but contains {} null(s)",
                    k, inner, node.null_count
                )));
            }
        }
    }
    Ok(())
}

/// Arrow type of a flat format string, for the types morloc declares.
fn format_to_datatype(f: &str) -> Option<DataType> {
    Some(match f {
        "b" => DataType::Boolean,
        "c" => DataType::Int8,
        "s" => DataType::Int16,
        "i" => DataType::Int32,
        "l" => DataType::Int64,
        "C" => DataType::UInt8,
        "S" => DataType::UInt16,
        "I" => DataType::UInt32,
        "L" => DataType::UInt64,
        "f" => DataType::Float32,
        "g" => DataType::Float64,
        "u" => DataType::Utf8,
        "U" => DataType::LargeUtf8,
        "vu" => DataType::Utf8View,
        _ => return None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use arrow_array::builder::{Int64Builder, ListBuilder, StringBuilder};
    use arrow_array::{
        BooleanArray, Float32Array, Int64Array, LargeStringArray, StringArray, TimestampMicrosecondArray,
        UInt64Array,
    };

    fn fixture() -> RecordBatch {
        let mut lb = ListBuilder::new(Int64Builder::new());
        lb.append_value([Some(1), Some(2)]);
        lb.append_null();
        lb.append_value([Some(3)]);
        let li = lb.finish();
        let mut sb = StringBuilder::new();
        sb.append_value("");
        sb.append_null();
        sb.append_value("h\u{e9}llo");
        let s = sb.finish();
        let schema = ArrowSchema::new(vec![
            Field::new("b", DataType::Boolean, true),
            Field::new("i", DataType::Int64, true),
            Field::new("u", DataType::UInt64, true),
            Field::new("f", DataType::Float32, true),
            Field::new("s", DataType::Utf8, true),
            Field::new("ls", DataType::LargeUtf8, true),
            Field::new("ts", DataType::Timestamp(arrow_schema::TimeUnit::Microsecond, Some("UTC".into())), true),
            Field::new("li", DataType::List(Arc::new(Field::new("item", DataType::Int64, true))), true),
        ]);
        RecordBatch::try_new(
            Arc::new(schema),
            vec![
                Arc::new(BooleanArray::from(vec![Some(true), None, Some(false)])),
                Arc::new(Int64Array::from(vec![Some(i64::MIN), None, Some(i64::MAX)])),
                Arc::new(UInt64Array::from(vec![Some(0), None, Some(u64::MAX)])),
                Arc::new(Float32Array::from(vec![Some(0.5), None, Some(100.125)])),
                Arc::new(s),
                Arc::new(LargeStringArray::from(vec![Some("a"), None, Some("\u{65e5}")])),
                Arc::new(
                    TimestampMicrosecondArray::from(vec![Some(0), None, Some(1577836800000000)])
                        .with_timezone("UTC"),
                ),
                Arc::new(li),
            ],
        )
        .unwrap()
    }

    fn with_shm<F: FnOnce()>(f: F) {
        let _guard = crate::init_test_shm();
        f();
    }

    #[test]
    fn round_trip_preserves_everything() {
        with_shm(|| {
            let batch = fixture();
            let rel = write_batch(&batch, None).unwrap();
            let base = shm::rel2abs(rel).unwrap();
            let back = unsafe { shm_to_batch(base as *const ArrowShmHeader) }.unwrap();
            assert_eq!(batch, back);
            assert_eq!(back.column(4).null_count(), 1);
        });
    }

    #[test]
    fn slices_are_compacted() {
        with_shm(|| {
            let batch = fixture().slice(1, 2);
            let rel = write_batch(&batch, None).unwrap();
            let base = shm::rel2abs(rel).unwrap();
            let back = unsafe { shm_to_batch(base as *const ArrowShmHeader) }.unwrap();
            assert_eq!(batch, back);
            let view = unsafe { BlockView::open(base as *const ArrowShmHeader) }.unwrap();
            assert_eq!(view.node(0).length, 2);
            assert_eq!(view.node(0).offset, 0);
        });
    }

    #[test]
    fn zero_rows_round_trip() {
        with_shm(|| {
            let batch = fixture().slice(0, 0);
            let rel = write_batch(&batch, None).unwrap();
            let base = shm::rel2abs(rel).unwrap();
            let back = unsafe { shm_to_batch(base as *const ArrowShmHeader) }.unwrap();
            assert_eq!(back.num_rows(), 0);
            assert_eq!(back.schema(), batch.schema());
        });
    }

    #[test]
    fn dictionaries_are_decoded_to_their_value_type() {
        with_shm(|| {
            let keys = arrow_array::Int32Array::from(vec![0, 1, 0]);
            let values = StringArray::from(vec!["a", "b"]);
            let dict = arrow_array::DictionaryArray::new(keys, Arc::new(values));
            let schema = ArrowSchema::new(vec![Field::new("d", dict.data_type().clone(), true)]);
            let batch = RecordBatch::try_new(Arc::new(schema), vec![Arc::new(dict)]).unwrap();
            let rel = write_batch(&batch, None).unwrap();
            let base = shm::rel2abs(rel).unwrap();
            let back = unsafe { shm_to_batch(base as *const ArrowShmHeader) }.unwrap();
            let col = back.column(0).as_any().downcast_ref::<StringArray>().unwrap();
            assert_eq!(col.value(0), "a");
            assert_eq!(col.value(1), "b");
            assert_eq!(col.value(2), "a");

            let keys = arrow_array::Int32Array::from(vec![0, 1]);
            let values = Int64Array::from(vec![10, 20]);
            let dict = arrow_array::DictionaryArray::new(keys, Arc::new(values));
            let schema = ArrowSchema::new(vec![Field::new("d", dict.data_type().clone(), true)]);
            let batch = RecordBatch::try_new(Arc::new(schema), vec![Arc::new(dict)]).unwrap();
            let rel = write_batch(&batch, None).unwrap();
            let base = shm::rel2abs(rel).unwrap();
            let back = unsafe { shm_to_batch(base as *const ArrowShmHeader) }.unwrap();
            let col = back.column(0).as_any().downcast_ref::<Int64Array>().unwrap();
            assert_eq!(col.values(), &[10, 20]);
        });
    }

    #[test]
    fn declared_schema_casts_and_checks_nulls() {
        with_shm(|| {
            let schema = ArrowSchema::new(vec![Field::new("x", DataType::Int32, true)]);
            let batch = RecordBatch::try_new(
                Arc::new(schema),
                vec![Arc::new(arrow_array::Int32Array::from(vec![Some(1), Some(2)]))],
            )
            .unwrap();
            let declared = Schema {
                serial_type: SerialType::Table,
                size: 1,
                width: std::mem::size_of::<shm::Array>(),
                offsets: vec![],
                hint: None,
                parameters: vec![Schema::primitive(SerialType::Sint64)],
                keys: vec!["x".into()],
                name: None,
            };
            let rel = write_batch(&batch, Some(&declared)).unwrap();
            let base = shm::rel2abs(rel).unwrap();
            unsafe { validate(base as *const ArrowShmHeader, &declared) }.unwrap();
            let back = unsafe { shm_to_batch(base as *const ArrowShmHeader) }.unwrap();
            assert_eq!(back.column(0).data_type(), &DataType::Int64);
            // The producer's nullable flag is kept; only actual nulls are refused.
            assert!(back.schema().field(0).is_nullable());

            let with_null = RecordBatch::try_new(
                batch.schema(),
                vec![Arc::new(arrow_array::Int32Array::from(vec![Some(1), None]))],
            )
            .unwrap();
            let e = write_batch(&with_null, Some(&declared)).unwrap_err();
            assert!(format!("{}", e).contains("non-nullable"));
        });
    }

    #[test]
    fn lossy_casts_are_refused() {
        with_shm(|| {
            let declared = Schema {
                serial_type: SerialType::Table,
                size: 1,
                width: std::mem::size_of::<shm::Array>(),
                offsets: vec![],
                hint: None,
                parameters: vec![Schema::primitive(SerialType::Sint64)],
                keys: vec!["x".into()],
                name: None,
            };
            let schema = Arc::new(ArrowSchema::new(vec![Field::new("x", DataType::Float64, false)]));
            let exact = RecordBatch::try_new(
                schema.clone(),
                vec![Arc::new(arrow_array::Float64Array::from(vec![1.0, 2.0]))],
            )
            .unwrap();
            assert!(write_batch(&exact, Some(&declared)).is_ok());
            let lossy = RecordBatch::try_new(
                schema,
                vec![Arc::new(arrow_array::Float64Array::from(vec![1.5, 2.0]))],
            )
            .unwrap();
            let e = write_batch(&lossy, Some(&declared)).unwrap_err();
            assert!(format!("{}", e).contains("losslessly"));
        });
    }

    #[test]
    fn unchanged_view_is_borrowed_and_changed_view_is_not() {
        with_shm(|| {
            let batch = fixture();
            let rel = write_batch(&batch, None).unwrap();
            let base = shm::rel2abs(rel).unwrap();

            let mut s = FFI_ArrowSchema::empty();
            let mut a = FFI_ArrowArray::empty();
            unsafe { shm_to_ffi_owned(base as *const ArrowShmHeader, true, &mut s, &mut a) }.unwrap();
            let got = unsafe { try_borrow(&a as *const _, &s as *const _, None) };
            assert_eq!(got, Some(rel));
            let _ = shm::shfree(base);

            // Renaming one column must defeat the match.
            let raw = unsafe { &mut *(&mut s as *mut FFI_ArrowSchema as *mut RawSchema) };
            let child = unsafe { &mut **raw.children.add(0) };
            let other = std::ffi::CString::new("renamed").unwrap();
            let saved = child.name;
            child.name = other.as_ptr();
            let got = unsafe { try_borrow(&a as *const _, &s as *const _, None) };
            assert_eq!(got, None);
            child.name = saved;

            // A slice of the view must defeat it too.
            let arr = unsafe { &mut *(&mut a as *mut FFI_ArrowArray as *mut RawArray) };
            arr.offset = 1;
            arr.length -= 1;
            let got = unsafe { try_borrow(&a as *const _, &s as *const _, None) };
            assert_eq!(got, None);
            arr.offset = 0;
            arr.length += 1;
            release_roots(&mut s, &mut a, false);
        });
    }

    #[test]
    fn root_nulls_are_refused() {
        with_shm(|| {
            let inner = Int64Array::from(vec![1, 2]);
            let s = StructArray::new(
                vec![Arc::new(Field::new("x", DataType::Int64, true))].into(),
                vec![Arc::new(inner)],
                Some(arrow_buffer::NullBuffer::from(vec![true, false])),
            );
            let (mut a, sc) = arrow_array::ffi::to_ffi(&s.into_data()).unwrap();
            let e = unsafe { ffi_to_batch(&mut a as *mut _, &sc as *const _) }.unwrap_err();
            assert!(format!("{}", e).contains("rows cannot themselves be null"));
        });
    }

    #[test]
    fn changed_metadata_is_not_borrowed() {
        with_shm(|| {
            let batch = fixture();
            let rel = write_batch(&batch, None).unwrap();
            let base = shm::rel2abs(rel).unwrap();
            let mut s = FFI_ArrowSchema::empty();
            let mut a = FFI_ArrowArray::empty();
            unsafe { shm_to_ffi_owned(base as *const ArrowShmHeader, true, &mut s, &mut a) }.unwrap();
            assert_eq!(unsafe { try_borrow(&a as *const _, &s as *const _, None) }, Some(rel));
            let _ = shm::shfree(base);
            // Attach one metadata pair to the first column: n=1, key "k", value "v".
            let blob: Vec<u8> = {
                let mut b = Vec::new();
                b.extend_from_slice(&1i32.to_ne_bytes());
                b.extend_from_slice(&1i32.to_ne_bytes());
                b.push(b'k');
                b.extend_from_slice(&1i32.to_ne_bytes());
                b.push(b'v');
                b
            };
            let raw = unsafe { &mut *(&mut s as *mut FFI_ArrowSchema as *mut RawSchema) };
            let child = unsafe { &mut **raw.children.add(0) };
            let saved = child.metadata;
            child.metadata = blob.as_ptr() as *const c_char;
            assert_eq!(unsafe { try_borrow(&a as *const _, &s as *const _, None) }, None);
            child.metadata = saved;
            release_roots(&mut s, &mut a, false);
        });
    }

    #[test]
    fn ffi_round_trip() {
        with_shm(|| {
            let batch = fixture();
            let (mut a, s) = arrow_array::ffi::to_ffi(&StructArray::from(batch.clone()).into_data()).unwrap();
            let adopted = unsafe { ffi_to_batch(&mut a as *mut _, &s as *const _) }.unwrap();
            assert!(a.is_released());
            assert_eq!(adopted, batch);
        });
    }

    /// Release both roots of a view, in the given order.
    fn release_roots(s: &mut FFI_ArrowSchema, a: &mut FFI_ArrowArray, array_first: bool) {
        unsafe {
            let rs = s as *mut FFI_ArrowSchema as *mut RawSchema;
            let ra = a as *mut FFI_ArrowArray as *mut RawArray;
            let mut drop_s = || { if let Some(f) = (*rs).release { f(s as *mut FFI_ArrowSchema) } };
            let mut drop_a = || { if let Some(f) = (*ra).release { f(a as *mut FFI_ArrowArray) } };
            if array_first { drop_a(); drop_s(); } else { drop_s(); drop_a(); }
        }
    }

    #[test]
    fn an_acquiring_view_holds_one_reference_until_both_roots_release() {
        with_shm(|| {
            for array_first in [false, true] {
                let rel = write_batch(&fixture(), None).unwrap();
                let base = shm::rel2abs(rel).unwrap();
                assert_eq!(shm::reference_count(base), Some(1));

                let mut s = FFI_ArrowSchema::empty();
                let mut a = FFI_ArrowArray::empty();
                unsafe { shm_to_ffi_owned(base as *const ArrowShmHeader, true, &mut s, &mut a) }.unwrap();
                assert_eq!(shm::reference_count(base), Some(2), "the view takes its own reference");

                release_roots(&mut s, &mut a, array_first);
                assert_eq!(shm::reference_count(base), Some(1), "releasing the view gives it back");
                let _ = shm::shfree(base);
            }
        });
    }

    #[test]
    fn a_view_released_on_another_thread_gives_its_reference_back() {
        with_shm(|| {
            let rel = write_batch(&fixture(), None).unwrap();
            let base = shm::rel2abs(rel).unwrap();
            let mut s = FFI_ArrowSchema::empty();
            let mut a = FFI_ArrowArray::empty();
            unsafe { shm_to_ffi_owned(base as *const ArrowShmHeader, true, &mut s, &mut a) }.unwrap();
            assert_eq!(shm::reference_count(base), Some(2));

            // A language that frees its objects on a finalizer thread
            // releases the view from a thread that never imported it.
            let moved = (s, a);
            std::thread::spawn(move || {
                let (mut s, mut a) = moved;
                release_roots(&mut s, &mut a, true);
            })
            .join()
            .unwrap();
            assert_eq!(shm::reference_count(base), Some(1));
            let _ = shm::shfree(base);
        });
    }

    #[test]
    fn an_adopting_view_frees_the_block_it_was_given() {
        with_shm(|| {
            let rel = write_batch(&fixture(), None).unwrap();
            let base = shm::rel2abs(rel).unwrap();
            let mut s = FFI_ArrowSchema::empty();
            let mut a = FFI_ArrowArray::empty();
            // The materialised case: the caller's only reference passes to
            // the view.
            unsafe { shm_to_ffi_owned(base as *const ArrowShmHeader, false, &mut s, &mut a) }.unwrap();
            assert_eq!(shm::reference_count(base), Some(1));
            release_roots(&mut s, &mut a, false);
            assert_eq!(shm::reference_count(base), Some(0), "the last view frees the block");
        });
    }

    #[test]
    fn a_third_root_release_is_a_no_op() {
        with_shm(|| {
            let rel = write_batch(&fixture(), None).unwrap();
            let base = shm::rel2abs(rel).unwrap();
            let mut s = FFI_ArrowSchema::empty();
            let mut a = FFI_ArrowArray::empty();
            unsafe { shm_to_ffi_owned(base as *const ArrowShmHeader, true, &mut s, &mut a) }.unwrap();
            let arena = unsafe { (*(&s as *const FFI_ArrowSchema as *const RawSchema)).private_data };
            release_roots(&mut s, &mut a, false);
            assert_eq!(shm::reference_count(base), Some(1));
            // A consumer that releases a struct it was told to consider
            // consumed must not take the block from whoever still holds it.
            unsafe { release_arena(arena) };
            assert_eq!(shm::reference_count(base), Some(1));
            let _ = shm::shfree(base);
        });
    }

    #[test]
    fn a_borrow_candidate_lives_exactly_as_long_as_its_view() {
        with_shm(|| {
            let rel = write_batch(&fixture(), None).unwrap();
            let base = shm::rel2abs(rel).unwrap();
            let mut s = FFI_ArrowSchema::empty();
            let mut a = FFI_ArrowArray::empty();
            unsafe { shm_to_ffi_owned(base as *const ArrowShmHeader, true, &mut s, &mut a) }.unwrap();

            // The view is what makes the block borrowable: returning it
            // unchanged passes the block through instead of copying.
            let got = unsafe { try_borrow(&a as *const _, &s as *const _, None) };
            assert_eq!(got, Some(rel));
            assert_eq!(shm::reference_count(base), Some(3), "a borrow takes a packet reference");
            let _ = shm::shfree(base);

            let view_copy = (unsafe { ptr::read(&s) }, unsafe { ptr::read(&a) });
            release_roots(&mut s, &mut a, false);
            // With no view left there is nothing to recognise the table
            // against, so a table of the same shape is copied, not borrowed.
            let (s2, a2) = view_copy;
            assert_eq!(unsafe { try_borrow(&a2 as *const _, &s2 as *const _, None) }, None);
            let _ = shm::shfree(base);
        });
    }

    #[test]
    fn a_forked_child_cannot_release_its_parent_s_reference() {
        with_shm(|| {
            let rel = write_batch(&fixture(), None).unwrap();
            let base = shm::rel2abs(rel).unwrap();
            let mut s = FFI_ArrowSchema::empty();
            let mut a = FFI_ArrowArray::empty();
            unsafe { shm_to_ffi_owned(base as *const ArrowShmHeader, true, &mut s, &mut a) }.unwrap();
            let arena = unsafe { (*(&s as *const FFI_ArrowSchema as *const RawSchema)).private_data };
            // User code that forks inside a worker inherits live views; a
            // finalizer in the child must not decrement a count the parent
            // still owns.
            unsafe { (*(arena as *mut ImportArena)).owner_pid += 1 };
            release_roots(&mut s, &mut a, false);
            assert_eq!(shm::reference_count(base), Some(2));
            let _ = shm::shfree(base);
            let _ = shm::shfree(base);
        });
    }

    #[test]
    fn failed_import_leaves_the_out_structs_released() {
        with_shm(|| {
            let batch = fixture();
            let rel = write_batch(&batch, None).unwrap();
            let base = shm::rel2abs(rel).unwrap();
            let mut s = FFI_ArrowSchema::empty();
            let mut a = FFI_ArrowArray::empty();
            // Garbage in the out structs must not survive a failed import:
            // a caller that tests `release` afterwards would call it.
            unsafe {
                (*(&mut s as *mut FFI_ArrowSchema as *mut RawSchema)).release = Some(release_child_schema);
                (*(&mut a as *mut FFI_ArrowArray as *mut RawArray)).release = Some(release_child_array);
                (*(base as *mut ArrowShmHeader)).magic = 0;
            }
            assert!(unsafe { shm_to_ffi(base as *const ArrowShmHeader, &mut s, &mut a) }.is_err());
            assert!(unsafe { (*(&s as *const FFI_ArrowSchema as *const RawSchema)).release.is_none() });
            assert!(a.is_released());
        });
    }

    /// The block's node descriptor `k`, for corrupting a written block.
    unsafe fn node_mut(base: *mut u8, k: usize) -> &'static mut ArrowNodeDesc {
        let h = &*(base as *const ArrowShmHeader);
        &mut *(base.add(h.nodes_offset as usize) as *mut ArrowNodeDesc).add(k)
    }

    fn open_err(base: *mut u8) -> String {
        format!("{}", unsafe { BlockView::open(base as *const ArrowShmHeader) }.err().expect("block accepted"))
    }

    #[test]
    fn node_lengths_are_tied_to_buffer_sizes() {
        with_shm(|| {
            // Fixture columns are root(0) b(1) i(2) u(3) f(4) s(5) ls(6) ts(7) li(8) li.item(9).
            let write = || {
                let rel = write_batch(&fixture(), None).unwrap();
                shm::rel2abs(rel).unwrap()
            };
            let base = write();
            assert!(unsafe { BlockView::open(base as *const ArrowShmHeader) }.is_ok());

            // A fixed-width column whose length outruns its data buffer.
            unsafe { node_mut(base, 2).length = 1 << 20 };
            assert!(open_err(base).contains("node 2"), "{}", open_err(base));

            // A bitmap column whose length outruns its bitmaps.
            let base = write();
            unsafe { node_mut(base, 1).length = 1 << 20 };
            assert!(open_err(base).contains("node 1"));

            // A variable-width column whose offsets buffer is too short
            // for its length.
            let base = write();
            unsafe { node_mut(base, 5).length = 1 << 20 };
            assert!(open_err(base).contains("node 5"));

            // A variable-width column whose last offset points past its
            // data buffer.
            let base = write();
            unsafe {
                let n = node_mut(base, 5);
                let h = &*(base as *const ArrowShmHeader);
                let bufs = base.add(h.buffers_offset as usize) as *const ArrowBufferDesc;
                let offsets = &*bufs.add(n.buffer_index as usize + 1);
                let last = base.add(offsets.offset as usize + n.length as usize * 4) as *mut i32;
                *last = 1 << 20;
            }
            assert!(open_err(base).contains("node 5"));

            // A list whose child is shorter than its last offset.
            let base = write();
            unsafe { node_mut(base, 9).length = 0 };
            assert!(open_err(base).contains("node 9") || open_err(base).contains("node 8"));

            // A root whose children are shorter than it.
            let base = write();
            unsafe {
                node_mut(base, 0).length = 4;
                (*(base as *mut ArrowShmHeader)).n_rows = 4;
            }
            assert!(open_err(base).contains("node 0"), "{}", open_err(base));

            // A null count with no validity bitmap to back it.
            let base = write();
            unsafe {
                let h = &*(base as *const ArrowShmHeader);
                let n = node_mut(base, 2);
                let bufs = base.add(h.buffers_offset as usize) as *mut ArrowBufferDesc;
                (*bufs.add(n.buffer_index as usize)).kind = BUF_NULL;
            }
            assert!(open_err(base).contains("node 2"));

            // A node whose length disagrees with the row count.
            let base = write();
            unsafe { (*(base as *mut ArrowShmHeader)).n_rows = 7 };
            assert!(open_err(base).contains("row"));
        });
    }

    #[test]
    fn metadata_blobs_are_bounded_by_the_string_table() {
        with_shm(|| {
            let mut md = std::collections::HashMap::new();
            md.insert("k".to_string(), "v".to_string());
            let schema = ArrowSchema::new(vec![Field::new("x", DataType::Int64, true).with_metadata(md)]);
            let batch = RecordBatch::try_new(Arc::new(schema), vec![Arc::new(Int64Array::from(vec![1, 2]))]).unwrap();
            let rel = write_batch(&batch, None).unwrap();
            let base = shm::rel2abs(rel).unwrap();
            let back = unsafe { shm_to_batch(base as *const ArrowShmHeader) }.unwrap();
            assert_eq!(back.schema().field(0).metadata().get("k").map(|s| s.as_str()), Some("v"));

            // Inflate the blob's pair count so a walk over it would leave
            // the string table.
            unsafe {
                let h = &*(base as *const ArrowShmHeader);
                let n = node_mut(base, 1);
                assert!(n.metadata_offset != 0);
                let blob = base.add(h.strtab_offset as usize + n.metadata_offset as usize) as *mut i32;
                ptr::write_unaligned(blob, 1 << 30);
            }
            assert!(open_err(base).contains("metadata"), "{}", open_err(base));
        });
    }

    #[test]
    fn declared_str_accepts_any_text_and_casts_numbers() {
        with_shm(|| {
            let declared = Schema::table(vec![Schema::primitive(SerialType::String)], vec!["id".into()]);
            let text = |dt: DataType, col: ArrayRef| {
                let schema = ArrowSchema::new(vec![Field::new("id", dt, true)]);
                RecordBatch::try_new(Arc::new(schema), vec![col]).unwrap()
            };
            // Every text form passes through unchanged.
            for b in [
                text(DataType::Utf8, Arc::new(StringArray::from(vec!["7", "8"]))),
                text(DataType::LargeUtf8, Arc::new(LargeStringArray::from(vec!["7", "8"]))),
                text(DataType::Utf8View, Arc::new(arrow_array::StringViewArray::from(vec!["7", "8"]))),
            ] {
                let rel = write_batch(&b, Some(&declared)).unwrap();
                let base = shm::rel2abs(rel).unwrap();
                unsafe { validate(base as *const ArrowShmHeader, &declared) }.unwrap();
                let view = unsafe { BlockView::open(base as *const ArrowShmHeader) }.unwrap();
                let expected = FFI_ArrowSchema::try_from(b.column(0).data_type()).unwrap();
                assert_eq!(view.string_str(view.column_node(0).format_offset), expected.format());
            }
            // A numeric identifier is rendered as text.
            let ints = text(DataType::Int64, Arc::new(Int64Array::from(vec![7, 8])));
            let rel = write_batch(&ints, Some(&declared)).unwrap();
            let base = shm::rel2abs(rel).unwrap();
            let back = unsafe { shm_to_batch(base as *const ArrowShmHeader) }.unwrap();
            let col = back.column(0).as_any().downcast_ref::<StringArray>().unwrap();
            assert_eq!(col.value(0), "7");
            assert_eq!(col.value(1), "8");
        });
    }
}
