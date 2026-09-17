//! MessagePack <-> Voidstar conversion.
//!
//! Replaces serialize.c + mpack.c. Uses the `rmp` crate for MessagePack I/O.
//! The voidstar binary format is morloc-specific (Array/Tensor structs with relptrs).

use crate::error::MorlocError;
use crate::recur::Resolver;
use crate::schema::{Schema, SerialType};
use crate::walk::{self, Frame, Stack, Visit, Walker};
use crate::shm::{self, AbsPtr, Array, RELNULL};

// ── Voidstar -> MessagePack ────────────────────────────────────────────────

/// Serialize voidstar data to MessagePack bytes.
pub fn pack_with_schema(ptr: AbsPtr, schema: &Schema) -> Result<Vec<u8>, MorlocError> {
    let mut buf = Vec::with_capacity(256);
    let mut w = PackWalk { res: Resolver::new(schema), buf: &mut buf };
    let mut st = Stack::new();
    st.enter(schema, ptr, ());
    walk::run(&mut w, &mut st)?;
    Ok(buf)
}

/// Appends a value's msgpack encoding in one pre-order pass; msgpack has
/// no closing tokens, so a container needs no post action.
struct PackWalk<'r, 'b> {
    res: Resolver<'r>,
    buf: &'b mut Vec<u8>,
}

impl<'r, 'b> PackWalk<'r, 'b> {
    fn child(
        &mut self,
        st: &mut Stack<()>,
        f: &Frame<()>,
        idx: usize,
        s: &'r Schema,
        data: *const u8,
    ) -> Result<Visit, MorlocError> {
        if self.res.flat(s) {
            self.step(st, Frame::new(s, data, ()))?;
            Ok(Visit::Done)
        } else {
            walk::defer(self, st, f, idx, s, data, ());
            Ok(Visit::Deferred)
        }
    }
}

impl<'r, 'b> Walker<()> for PackWalk<'r, 'b> {
    fn step(&mut self, st: &mut Stack<()>, f: Frame<()>) -> Result<(), MorlocError> {
        // SAFETY: frames hold nodes of the tree the resolver was built from;
        // `data` points at a value laid out as that schema describes, and
        // every read is within the bounds the schema and headers give.
        let s: &'r Schema = self.res.resolve(unsafe { &*f.schema })?;
        let data = f.data;
        let buf = &mut *self.buf;
        unsafe {
        match s.serial_type {
            SerialType::Nil => {
                rmp::encode::write_nil(buf)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack nil: {}", e)))?;
            }
            SerialType::Bool => {
                let v = *data != 0;
                rmp::encode::write_bool(buf, v)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack bool: {}", e)))?;
            }
            SerialType::Uint8 => {
                rmp::encode::write_uint(buf, *data as u64)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack uint: {}", e)))?;
            }
            // An enum travels msgpack as its ordinal, not its name.
            // msgpack is the machine format -- it carries packets and
            // on-disk values -- and spelling out "A"/"C"/"G"/"T" would
            // multiply a genome-sized [DNA] several-fold. The names are
            // never lost: the schema string travels with the packet, and
            // JSON (the human- and LLM-facing format) does render them.
            SerialType::Enum => {
                rmp::encode::write_uint(buf, *data as u64)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack enum: {}", e)))?;
            }
            SerialType::Uint16 => {
                rmp::encode::write_uint(buf, *(data as *const u16) as u64)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack uint: {}", e)))?;
            }
            SerialType::Uint32 => {
                rmp::encode::write_uint(buf, *(data as *const u32) as u64)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack uint: {}", e)))?;
            }
            SerialType::Uint64 => {
                rmp::encode::write_uint(buf, *(data as *const u64))
                    .map_err(|e| MorlocError::Serialization(format!("msgpack uint: {}", e)))?;
            }
            SerialType::Sint8 => {
                rmp::encode::write_sint(buf, *(data as *const i8) as i64)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack sint: {}", e)))?;
            }
            SerialType::Sint16 => {
                rmp::encode::write_sint(buf, *(data as *const i16) as i64)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack sint: {}", e)))?;
            }
            SerialType::Sint32 => {
                rmp::encode::write_sint(buf, *(data as *const i32) as i64)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack sint: {}", e)))?;
            }
            SerialType::Sint64 => {
                rmp::encode::write_sint(buf, *(data as *const i64))
                    .map_err(|e| MorlocError::Serialization(format!("msgpack sint: {}", e)))?;
            }
            SerialType::Float32 => {
                let f = *(data as *const f32) as f64;
                rmp::encode::write_f64(buf, f)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack float: {}", e)))?;
            }
            SerialType::Float64 => {
                let f = *(data as *const f64);
                rmp::encode::write_f64(buf, f)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack float: {}", e)))?;
            }
            SerialType::Int => {
                // Inline BigInt: [size, value_or_relptr]
                let size = *(data as *const usize);
                if size <= 1 {
                    let val = *(data.add(8) as *const i64);
                    rmp::encode::write_sint(buf, if size == 0 { 0 } else { val })
                        .map_err(|e| MorlocError::Serialization(format!("msgpack bigint: {}", e)))?;
                } else {
                    let relptr = *(data.add(std::mem::size_of::<usize>()) as *const shm::RelPtr);
                    let data = shm::rel2abs(relptr)?;
                    let bytes = std::slice::from_raw_parts(data, size * 8);
                    rmp::encode::write_bin_len(buf, bytes.len() as u32)
                        .map_err(|e| MorlocError::Serialization(format!("msgpack bigint: {}", e)))?;
                    buf.extend_from_slice(bytes);
                }
            }
            SerialType::String => {
                let arr = &*(data as *const Array);
                rmp::encode::write_str_len(buf, arr.size as u32)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack str: {}", e)))?;
                // An empty string carries no data block.
                if arr.size > 0 && arr.data != RELNULL {
                    let data = shm::rel2abs(arr.data)?;
                    buf.extend_from_slice(std::slice::from_raw_parts(data, arr.size));
                }
            }
            SerialType::IFile | SerialType::OStream | SerialType::IStream => {
                // Persistence to msgpack uses path form regardless of the
                // in-memory tag. TAG_HANDLE values look up the path via
                // the local SHM registry so a file on disk always carries
                // a path the next reader can `mlc_open` against.
                use morloc_runtime_types::stream_handle as sh;
                let field = data;
                let tag = sh::read_tag(field);
                let payload = sh::read_payload(field);
                let path: String = if tag == sh::TAG_PATH {
                    if payload == sh::RELNULL_PAYLOAD {
                        String::new()
                    } else {
                        let suballoc = shm::rel2abs(payload as shm::RelPtr)?;
                        let path_len = sh::read_path_size(suballoc) as usize;
                        let bytes = std::slice::from_raw_parts(suballoc.add(8), path_len);
                        std::str::from_utf8(bytes)
                            .map_err(|_| MorlocError::Serialization(
                                "msgpack stream-handle: path is not valid UTF-8".into(),
                            ))?
                            .to_string()
                    }
                } else if tag == sh::TAG_HANDLE {
                    crate::stream::handle_path(payload as i64)?
                } else {
                    return Err(MorlocError::Serialization(format!(
                        "msgpack stream-handle: unsupported tag {}", tag,
                    )));
                };
                let bytes = path.as_bytes();
                rmp::encode::write_str_len(buf, bytes.len() as u32)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack str: {}", e)))?;
                buf.extend_from_slice(bytes);
            }
            // A variant travels as a two-element array [tag, payload],
            // a direct transcription of the voidstar slot. The payload is
            // the arm's field tuple, or nil for an arm with no fields.
            SerialType::Variant => {
                if f.idx > 0 {
                    return Ok(());
                }
                let tag = *data;
                let arm = s.parameters.get(tag as usize).ok_or_else(|| {
                    MorlocError::Serialization(format!(
                        "variant tag {} is out of range; the type has {} arms",
                        tag, s.size
                    ))
                })?;
                rmp::encode::write_array_len(buf, 2)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack variant: {}", e)))?;
                rmp::encode::write_uint(buf, tag as u64)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack variant tag: {}", e)))?;
                let payload = *(data.add(8) as *const shm::RelPtr);
                if arm.size == 0 || payload == shm::RELNULL {
                    rmp::encode::write_nil(buf).map_err(|e| {
                        MorlocError::Serialization(format!("msgpack variant payload: {}", e))
                    })?;
                } else {
                    let inner = shm::rel2abs(payload)?;
                    self.child(st, &f, 0, arm, inner)?;
                }
            }
            SerialType::Array => {
                let arr = &*(data as *const Array);
                let elem_schema = &s.parameters[0];
                let elem_width = elem_schema.width;
                if f.idx == 0 {
                    rmp::encode::write_array_len(buf, arr.size as u32)
                        .map_err(|e| MorlocError::Serialization(format!("msgpack array: {}", e)))?;
                }
                if arr.size == 0 || arr.data == RELNULL {
                    return Ok(());
                }
                let elems = shm::rel2abs(arr.data)?;
                let flat_elem = self.res.flat(elem_schema);
                for i in f.idx..arr.size {
                    let p = elems.add(i * elem_width);
                    if flat_elem {
                        self.step(st, Frame::new(elem_schema, p, ()))?;
                    } else if self.child(st, &f, i, elem_schema, p)? == Visit::Deferred {
                        return Ok(());
                    }
                }
            }
            SerialType::Tuple | SerialType::Map => {
                if f.idx == 0 {
                    rmp::encode::write_array_len(buf, s.parameters.len() as u32)
                        .map_err(|e| MorlocError::Serialization(format!("msgpack tuple: {}", e)))?;
                }
                for i in f.idx..s.parameters.len() {
                    if self.child(st, &f, i, &s.parameters[i], data.add(s.offsets[i]))? == Visit::Deferred {
                        return Ok(());
                    }
                }
            }
            SerialType::Optional => {
                if f.idx > 0 {
                    return Ok(());
                }
                // The Optional slot is a single relptr: RELNULL for absent,
                // otherwise the relptr to T's body elsewhere in the buffer.
                let relptr = *(data as *const shm::RelPtr);
                if relptr == shm::RELNULL {
                    rmp::encode::write_nil(buf)
                        .map_err(|e| MorlocError::Serialization(format!("msgpack nil: {}", e)))?;
                } else {
                    let inner = shm::rel2abs(relptr)?;
                    self.child(st, &f, 0, &s.parameters[0], inner)?;
                }
            }
            SerialType::Table => {
                // Tables travel as Arrow IPC blobs through SHM, never as
                // msgpack. Hitting this case means a Table value got
                // routed to the msgpack path; the caller should have
                // dispatched via the Arrow C Data Interface instead.
                return Err(MorlocError::Serialization(
                    "Cannot msgpack-encode a Table; Tables use the Arrow IPC SHM wire path".into(),
                ));
            }
            SerialType::Recur => unreachable!("a back-reference resolves before it is stepped"),
        }
        }
        Ok(())
    }
}

// ── MessagePack -> Voidstar ────────────────────────────────────────────────

/// Deserialize MessagePack bytes into voidstar format in SHM.
pub fn unpack_with_schema(
    data: &[u8],
    schema: &Schema,
) -> Result<AbsPtr, MorlocError> {
    // Two-pass: first calculate size, then deserialize
    let size = calc_unpack_size(data, schema)?;
    let base = shm::shmalloc(size)?;
    // SAFETY: base is freshly allocated with `size` bytes.
    unsafe { std::ptr::write_bytes(base, 0, size) };

    // SAFETY: cursor starts at base + schema.width, within the allocated region.
    let mut w = UnpackWalk { res: Resolver::new(schema), cursor: unsafe { base.add(schema.width) }, reader: &data[..] };
    let mut st = Stack::new();
    st.enter(schema, base, ());
    walk::run(&mut w, &mut st)?;
    Ok(base)
}

/// Decodes msgpack into a single block, laying each value's blocks down
/// at a running cursor in one pre-order pass. A frame's `data` is the slot
/// the node is written into.
struct UnpackWalk<'r, 'd> {
    res: Resolver<'r>,
    cursor: AbsPtr,
    reader: &'d [u8],
}

impl<'r, 'd> UnpackWalk<'r, 'd> {
    fn child(
        &mut self,
        st: &mut Stack<()>,
        f: &Frame<()>,
        idx: usize,
        s: &'r Schema,
        slot: AbsPtr,
    ) -> Result<Visit, MorlocError> {
        if self.res.flat(s) {
            self.step(st, Frame::new(s, slot, ()))?;
            Ok(Visit::Done)
        } else {
            walk::defer(self, st, f, idx, s, slot, ());
            Ok(Visit::Deferred)
        }
    }
}

impl<'r, 'd> Walker<()> for UnpackWalk<'r, 'd> {
    fn step(&mut self, st: &mut Stack<()>, f: Frame<()>) -> Result<(), MorlocError> {
        use rmp::decode;
        // SAFETY: frames hold nodes of the tree the resolver was built from;
        // `data` and the cursor point into the one SHM allocation sized by
        // calc_unpack_size, and each write respects the slot's width.
        let s: &'r Schema = self.res.resolve(unsafe { &*f.schema })?;
        let ptr = f.data as *mut u8;
        unsafe {
        match s.serial_type {
            SerialType::Nil => {
                decode::read_nil(&mut self.reader)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack nil: {}", e)))?;
                *ptr = 0;
            }
            SerialType::Bool => {
                let v = decode::read_bool(&mut self.reader)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack bool: {}", e)))?;
                *ptr = v as u8;
            }
            SerialType::Uint8 | SerialType::Uint16 | SerialType::Uint32 | SerialType::Uint64
            | SerialType::Sint8 | SerialType::Sint16 | SerialType::Sint32 | SerialType::Sint64 => {
                unpack_int(ptr, s.serial_type, &mut self.reader)?;
            }
            // Read the ordinal and reject a tag no constructor claims.
            // This is the boundary check: a pool built against a different
            // version of the type fails here, naming the legal set, rather
            // than yielding a value that matches no arm deep in a manifold.
            SerialType::Enum => {
                let tag: i64 = decode::read_int(&mut self.reader).map_err(|e| {
                    MorlocError::Serialization(format!("msgpack enum tag: {}", e))
                })?;
                if tag < 0 || tag as usize >= s.size {
                    return Err(MorlocError::Serialization(format!(
                        "enum tag {} is out of range; the type has {} constructors ({})",
                        tag,
                        s.size,
                        s.keys.join(", ")
                    )));
                }
                *ptr = tag as u8;
            }
            SerialType::Float32 => {
                let f = read_float(&mut self.reader)?;
                *(ptr as *mut f32) = f as f32;
            }
            SerialType::Float64 => {
                let f = read_float(&mut self.reader)?;
                *(ptr as *mut f64) = f;
            }
            SerialType::Int => {
                // Inline BigInt: [size, value_or_relptr]
                let fields = ptr as *mut i64;
                let saved = self.reader;
                if let Ok(len) = rmp::decode::read_bin_len(&mut self.reader) {
                    // Overflow: multi-limb
                    let len = len as usize;
                    let nlimbs = len / 8;
                    *fields = nlimbs as i64;
                    *(fields.add(1)) = shm::abs2rel(self.cursor)? as i64;
                    if nlimbs > 0 && self.reader.len() >= len {
                        std::ptr::copy_nonoverlapping(self.reader.as_ptr(), self.cursor, len);
                        self.reader = &self.reader[len..];
                    }
                    self.cursor = self.cursor.add(nlimbs * 8);
                } else {
                    // Inline: single integer value
                    self.reader = saved;
                    let val: i64 = rmp::decode::read_int(&mut self.reader)
                        .map_err(|e| MorlocError::Serialization(format!("msgpack bigint: {}", e)))?;
                    *fields = 1;
                    *(fields.add(1)) = val;
                }
            }
            SerialType::String => {
                let len = decode::read_str_len(&mut self.reader)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack str len: {}", e)))?
                    as usize;
                let arr = &mut *(ptr as *mut Array);
                arr.size = len;
                arr.data = shm::abs2rel(self.cursor)?;

                // Read string bytes directly
                if len > 0 {
                    if self.reader.len() < len {
                        return Err(MorlocError::Serialization("msgpack str truncated".into()));
                    }
                    std::ptr::copy_nonoverlapping(self.reader.as_ptr(), self.cursor, len);
                    self.reader = &self.reader[len..];
                }
                self.cursor = self.cursor.add(len);
            }
            SerialType::IFile | SerialType::OStream | SerialType::IStream => {
                // msgpack carries the path as a string; we lay it down in
                // path form (TAG_PATH + `{size, bytes}` suballoc). The
                // receiver's language bridge will see the schema's F/O/I
                // code and re-open via `mlc_open` on first use.
                use morloc_runtime_types::stream_handle as sh;
                let len = decode::read_str_len(&mut self.reader)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack str len: {}", e)))?
                    as usize;
                let field = ptr as *mut u8;
                if len == 0 {
                    sh::write_field(field, sh::TAG_PATH, sh::RELNULL_PAYLOAD);
                } else {
                    if self.reader.len() < len {
                        return Err(MorlocError::Serialization("msgpack str truncated".into()));
                    }
                    let rel = shm::abs2rel(self.cursor)?;
                    let bytes = std::slice::from_raw_parts(self.reader.as_ptr(), len);
                    sh::write_path_suballoc(self.cursor, bytes);
                    sh::write_field(field, sh::TAG_PATH, rel as u64);
                    self.reader = &self.reader[len..];
                    self.cursor = self.cursor.add(sh::path_suballoc_size(len));
                }
            }
            SerialType::Array => {
                let elem_schema = &s.parameters[0];
                let elem_width = elem_schema.width;
                let arr = &mut *(ptr as *mut Array);
                if f.idx == 0 {
                    let n = decode::read_array_len(&mut self.reader)
                        .map_err(|e| MorlocError::Serialization(format!("msgpack array len: {}", e)))?
                        as usize;
                    arr.size = n;
                    // Align cursor for element data
                    // (bumps to 64 for primitive numerics for SIMD/BLAS)
                    let align = elem_schema.array_data_alignment();
                    self.cursor = shm::align_up(self.cursor as usize, align) as AbsPtr;
                    arr.data = shm::abs2rel(self.cursor)?;
                    self.cursor = self.cursor.add(n * elem_width);
                }
                let data_start = shm::rel2abs(arr.data)?;
                let flat_elem = self.res.flat(elem_schema);
                for i in f.idx..arr.size {
                    let p = data_start.add(i * elem_width);
                    if flat_elem {
                        self.step(st, Frame::new(elem_schema, p, ()))?;
                    } else if self.child(st, &f, i, elem_schema, p)? == Visit::Deferred {
                        return Ok(());
                    }
                }
            }
            SerialType::Tuple | SerialType::Map => {
                if f.idx == 0 {
                    let n = decode::read_array_len(&mut self.reader)
                        .map_err(|e| MorlocError::Serialization(format!("msgpack tuple len: {}", e)))?;
                    check_field_count(s, n as usize)?;
                }
                for i in f.idx..s.parameters.len() {
                    if self.child(st, &f, i, &s.parameters[i], ptr.add(s.offsets[i]))? == Visit::Deferred {
                        return Ok(());
                    }
                }
            }
            SerialType::Optional => {
                if f.idx > 0 {
                    return Ok(());
                }
                let inner_schema = &s.parameters[0];
                let relptr_slot = ptr as *mut shm::RelPtr;
                // Peek at the next byte to detect nil
                if !self.reader.is_empty() && self.reader[0] == 0xc0 {
                    // Absent: write RELNULL, consume nil byte
                    decode::read_nil(&mut self.reader)
                        .map_err(|e| MorlocError::Serialization(format!("msgpack nil: {}", e)))?;
                    *relptr_slot = shm::RELNULL;
                } else {
                    // Present: allocate inner T at the cursor, write its
                    // relptr into the Optional slot, then unpack T into
                    // the cursor region. The cursor advances past T's
                    // header width here; T's own walker advances it
                    // further for any sub-data it has.
                    let inner_align = inner_schema.alignment().max(1);
                    self.cursor = shm::align_up(self.cursor as usize, inner_align) as AbsPtr;
                    *relptr_slot = shm::abs2rel(self.cursor)?;
                    let inner_ptr = self.cursor;
                    self.cursor = self.cursor.add(inner_schema.width);
                    self.child(st, &f, 0, inner_schema, inner_ptr)?;
                }
            }
            SerialType::Variant => {
                if f.idx > 0 {
                    return Ok(());
                }
                // [tag, payload]. Read the tag, then unpack the arm the tag
                // selects into the cursor region and point the slot at it.
                decode::read_array_len(&mut self.reader).map_err(|e| {
                    MorlocError::Serialization(format!("msgpack variant array: {}", e))
                })?;
                let tag: i64 = decode::read_int(&mut self.reader).map_err(|e| {
                    MorlocError::Serialization(format!("msgpack variant tag: {}", e))
                })?;
                if tag < 0 || tag as usize >= s.size {
                    return Err(MorlocError::Serialization(format!(
                        "variant tag {} is out of range; the type has {} arms",
                        tag, s.size
                    )));
                }
                let arm = &s.parameters[tag as usize];
                *ptr = tag as u8;
                std::ptr::write_bytes(ptr.add(1), 0, 7);
                let relptr_slot = &mut *(ptr.add(8) as *mut shm::RelPtr);
                if !self.reader.is_empty() && self.reader[0] == 0xc0 {
                    decode::read_nil(&mut self.reader).map_err(|e| {
                        MorlocError::Serialization(format!("msgpack nil: {}", e))
                    })?;
                    *relptr_slot = shm::RELNULL;
                } else {
                    let inner_align = arm.alignment().max(1);
                    self.cursor = shm::align_up(self.cursor as usize, inner_align) as AbsPtr;
                    *relptr_slot = shm::abs2rel(self.cursor)?;
                    let inner_ptr = self.cursor;
                    self.cursor = self.cursor.add(arm.width);
                    self.child(st, &f, 0, arm, inner_ptr)?;
                }
            }
            SerialType::Table => {
                // See pack_data: Tables are not msgpack-serialised. Hitting
                // this case is a routing bug in the caller.
                return Err(MorlocError::Serialization(
                    "Cannot msgpack-decode a Table; Tables use the Arrow IPC SHM wire path".into(),
                ));
            }
            SerialType::Recur => unreachable!("a back-reference resolves before it is stepped"),
        }
        }
        Ok(())
    }
}

fn unpack_int(ptr: AbsPtr, st: SerialType, reader: &mut &[u8]) -> Result<(), MorlocError> {
    // Use rmp's generic read_int which handles all integer markers
    let val: i64 = rmp::decode::read_int(reader)
        .map_err(|e| MorlocError::Serialization(format!("msgpack int: {}", e)))?;

    // Range-check before narrowing. The msgpack stream may carry a wider
    // integer than the receiving schema slot (e.g. a producer that wrote
    // an i64-tagged value that fits in the wire width but not the target
    // type). Silent truncation here would defeat the json.rs check.
    let check_s = |lo: i64, hi: i64, name: &str| -> Result<i64, MorlocError> {
        if val < lo || val > hi {
            Err(MorlocError::Serialization(format!(
                "value {} out of range for {} (range {} to {})", val, name, lo, hi
            )))
        } else { Ok(val) }
    };
    let check_u = |hi: u64, name: &str| -> Result<u64, MorlocError> {
        if val < 0 || (val as u64) > hi {
            Err(MorlocError::Serialization(format!(
                "value {} out of range for {} (range 0 to {})", val, name, hi
            )))
        } else { Ok(val as u64) }
    };

    // Compute the narrowed values (with checks) outside the unsafe block.
    let i8v = if matches!(st, SerialType::Sint8)  { check_s(i8::MIN  as i64, i8::MAX  as i64, "I8" )? as i8  } else { 0 };
    let i16v = if matches!(st, SerialType::Sint16) { check_s(i16::MIN as i64, i16::MAX as i64, "I16")? as i16 } else { 0 };
    let i32v = if matches!(st, SerialType::Sint32) { check_s(i32::MIN as i64, i32::MAX as i64, "I32")? as i32 } else { 0 };
    let u8v  = if matches!(st, SerialType::Uint8)  { check_u(u8::MAX  as u64, "U8" )? as u8  } else { 0 };
    let u16v = if matches!(st, SerialType::Uint16) { check_u(u16::MAX as u64, "U16")? as u16 } else { 0 };
    let u32v = if matches!(st, SerialType::Uint32) { check_u(u32::MAX as u64, "U32")? as u32 } else { 0 };
    let u64v: u64 = if matches!(st, SerialType::Uint64) {
        if val < 0 {
            return Err(MorlocError::Serialization(format!(
                "value {} out of range for UInt64 (range 0 to {})", val, u64::MAX
            )));
        }
        val as u64
    } else { 0 };

    // SAFETY: ptr points to schema.width bytes in SHM; each cast writes exactly that width.
    unsafe {
        match st {
            SerialType::Sint8  => *(ptr as *mut i8)  = i8v,
            SerialType::Sint16 => *(ptr as *mut i16) = i16v,
            SerialType::Sint32 => *(ptr as *mut i32) = i32v,
            SerialType::Sint64 => *(ptr as *mut i64) = val,
            SerialType::Uint8  => *ptr               = u8v,
            SerialType::Uint16 => *(ptr as *mut u16) = u16v,
            SerialType::Uint32 => *(ptr as *mut u32) = u32v,
            SerialType::Uint64 => *(ptr as *mut u64) = u64v,
            _ => {}
        }
    }
    Ok(())
}

fn read_float(reader: &mut &[u8]) -> Result<f64, MorlocError> {
    let marker = rmp::decode::read_marker(reader)
        .map_err(|_| MorlocError::Serialization("msgpack float: unexpected EOF".into()))?;
    match marker {
        rmp::Marker::F32 => {
            let bits = read_be_u32(reader)?;
            Ok(f32::from_bits(bits) as f64)
        }
        rmp::Marker::F64 => {
            let bits = read_be_u64(reader)?;
            Ok(f64::from_bits(bits))
        }
        // Integer markers can appear for integer-valued floats - read the data manually
        rmp::Marker::FixPos(v) => Ok(v as f64),
        rmp::Marker::FixNeg(v) => Ok(v as f64),
        _ => {
            // For other integer encodings, read bytes manually
            let n = match marker {
                rmp::Marker::U8 => { read_byte(reader)? as f64 }
                rmp::Marker::U16 => { read_be_u16(reader)? as f64 }
                rmp::Marker::U32 => { read_be_u32(reader)? as f64 }
                rmp::Marker::U64 => { read_be_u64(reader)? as f64 }
                rmp::Marker::I8 => { read_byte(reader)? as i8 as f64 }
                rmp::Marker::I16 => { read_be_u16(reader)? as i16 as f64 }
                rmp::Marker::I32 => { read_be_u32(reader)? as i32 as f64 }
                rmp::Marker::I64 => { read_be_u64(reader)? as i64 as f64 }
                _ => {
                    return Err(MorlocError::Serialization(format!(
                        "unexpected msgpack marker {:?} for float", marker
                    )));
                }
            };
            Ok(n)
        }
    }
}

// Also fix read_f32/read_f64 - rmp's functions include the marker, but we already consumed it
// So we need to read the raw data bytes directly.

fn read_byte(reader: &mut &[u8]) -> Result<u8, MorlocError> {
    if reader.is_empty() { return Err(MorlocError::Serialization("unexpected EOF".into())); }
    let v = reader[0];
    *reader = &reader[1..];
    Ok(v)
}

fn read_be_u16(reader: &mut &[u8]) -> Result<u16, MorlocError> {
    if reader.len() < 2 { return Err(MorlocError::Serialization("unexpected EOF".into())); }
    let v = u16::from_be_bytes([reader[0], reader[1]]);
    *reader = &reader[2..];
    Ok(v)
}

fn read_be_u32(reader: &mut &[u8]) -> Result<u32, MorlocError> {
    if reader.len() < 4 { return Err(MorlocError::Serialization("unexpected EOF".into())); }
    let v = u32::from_be_bytes([reader[0], reader[1], reader[2], reader[3]]);
    *reader = &reader[4..];
    Ok(v)
}

fn read_be_u64(reader: &mut &[u8]) -> Result<u64, MorlocError> {
    if reader.len() < 8 { return Err(MorlocError::Serialization("unexpected EOF".into())); }
    let v = u64::from_be_bytes([reader[0], reader[1], reader[2], reader[3], reader[4], reader[5], reader[6], reader[7]]);
    *reader = &reader[8..];
    Ok(v)
}

// ── Size calculation for unpack ────────────────────────────────────────────

pub(crate) fn calc_unpack_size(data: &[u8], schema: &Schema) -> Result<usize, MorlocError> {
    let mut w = SizeWalk { res: Resolver::new(schema), reader: data, total: 0 };
    let mut st = Stack::new();
    st.enter(schema, std::ptr::null(), true);
    walk::run(&mut w, &mut st)?;
    Ok(w.total)
}

/// Sizes the block `unpack_with_schema` will lay a msgpack value into,
/// advancing through the stream as it goes. A frame's `x` says whether the
/// node's bytes count: a fixed-width field of a tuple is inside the tuple's
/// width already and only consumes its tokens.
struct SizeWalk<'r, 'd> {
    res: Resolver<'r>,
    reader: &'d [u8],
    total: usize,
}

impl<'r, 'd> SizeWalk<'r, 'd> {
    #[inline]
    fn add(&mut self, n: usize, counted: bool) {
        if counted {
            self.total += n;
        }
    }

    fn child(
        &mut self,
        st: &mut Stack<bool>,
        f: &Frame<bool>,
        idx: usize,
        s: &'r Schema,
        counted: bool,
    ) -> Result<Visit, MorlocError> {
        if self.res.flat(s) {
            self.step(st, Frame::new(s, std::ptr::null(), counted))?;
            Ok(Visit::Done)
        } else {
            walk::defer(self, st, f, idx, s, std::ptr::null(), counted);
            Ok(Visit::Deferred)
        }
    }
}

impl<'r, 'd> Walker<bool> for SizeWalk<'r, 'd> {
    fn step(&mut self, st: &mut Stack<bool>, f: Frame<bool>) -> Result<(), MorlocError> {
        // SAFETY: frames hold nodes of the tree the resolver was built from.
        let s: &'r Schema = self.res.resolve(unsafe { &*f.schema })?;
        match s.serial_type {
        SerialType::Nil => {
            rmp::decode::read_nil(&mut self.reader).ok();
            self.add(1, f.x) }
        SerialType::Bool => {
            rmp::decode::read_bool(&mut self.reader).ok();
            self.add(1, f.x) }
        SerialType::Sint8 | SerialType::Uint8 | SerialType::Enum => { skip_int(&mut self.reader)?; self.add(1, f.x) }
        SerialType::Sint16 | SerialType::Uint16 => { skip_int(&mut self.reader)?; self.add(2, f.x) }
        SerialType::Sint32 | SerialType::Uint32 | SerialType::Float32 => { skip_int(&mut self.reader)?; self.add(4, f.x) }
        SerialType::Sint64 | SerialType::Uint64 | SerialType::Float64 => { skip_int(&mut self.reader)?; self.add(8, f.x) }
        SerialType::Int => {
            // Inline BigInt: 16 bytes for common case, more for overflow
            let saved = self.reader;
            if let Ok(len) = rmp::decode::read_bin_len(&mut self.reader) {
                let len = len as usize;
                if self.reader.len() >= len { self.reader = &self.reader[len..]; }
                // Overflow: 16-byte header + alignment + limb data
                self.add(16 + std::mem::align_of::<u64>() - 1 + len, f.x);
            } else {
                self.reader = saved;
                skip_int(&mut self.reader)?;
                self.add(16, f.x); // Inline: just the [size, value] pair
            }
        }
        SerialType::String => {
            let len = rmp::decode::read_str_len(&mut self.reader)
                .map_err(|e| MorlocError::Serialization(format!("size calc str: {}", e)))?
                as usize;
            if self.reader.len() >= len { self.reader = &self.reader[len..]; }
            self.add(std::mem::size_of::<Array>() + len, f.x) }
        SerialType::IFile | SerialType::OStream | SerialType::IStream => {
            // Tagged stream-handle field: 16-byte inline + path suballoc
            // (`8 + path_len`). msgpack carries the path as a string.
            use morloc_runtime_types::stream_handle as sh;
            let len = rmp::decode::read_str_len(&mut self.reader)
                .map_err(|e| MorlocError::Serialization(format!("size calc str: {}", e)))?
                as usize;
            if self.reader.len() >= len { self.reader = &self.reader[len..]; }
            let suballoc = if len == 0 { 0 } else { sh::path_suballoc_size(len) };
            self.add(sh::STREAM_HANDLE_FIELD_SIZE + suballoc, f.x) }
        SerialType::Array => {
            if f.idx == 0 {
                let n = rmp::decode::read_array_len(&mut self.reader)
                    .map_err(|e| MorlocError::Serialization(format!("size calc array: {}", e)))?
                    as usize;
                let elem_schema = &s.parameters[0];
                // Alignment padding (bumps to 64 for primitive numerics for SIMD/BLAS)
                let head = shm::align_up(std::mem::size_of::<Array>(), elem_schema.array_data_alignment());
                self.add(head, f.x);
                // The element count is what the resumed loop needs.
                let mut g = f;
                g.idx = 0;
                return self.elements(st, g, n);
            }
            let n = f.data as usize;
            return self.elements(st, f, n);
        }
        SerialType::Tuple | SerialType::Map => {
            if f.idx == 0 {
                let n = rmp::decode::read_array_len(&mut self.reader)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack tuple len: {}", e)))?;
                check_field_count(s, n as usize)?;
                self.add(s.width, f.x);
            }
            for i in f.idx..s.parameters.len() {
                let field = &s.parameters[i];
                let counted = f.x && !field.is_fixed_width();
                if self.child(st, &f, i, field, counted)? == Visit::Deferred {
                    return Ok(());
                }
            }
        }
        SerialType::Variant => {
            if f.idx > 0 {
                return Ok(());
            }
            // The slot is 16 bytes; the arm's own data lands at the cursor.
            let saved = self.reader;
            self.add(s.width, f.x);
            if rmp::decode::read_array_len(&mut self.reader).is_ok() {
                if let Ok(tag) = rmp::decode::read_int::<i64, _>(&mut self.reader) {
                    if tag >= 0 && (tag as usize) < s.size {
                        let arm = &s.parameters[tag as usize];
                        if !self.reader.is_empty() && self.reader[0] == 0xc0 {
                            rmp::decode::read_nil(&mut self.reader).ok();
                        } else {
                            let inner_align = arm.alignment().max(1);
                            self.add(inner_align - 1, f.x);
                            self.child(st, &f, 0, arm, f.x)?;
                        }
                        return Ok(());
                    }
                }
            }
            self.reader = saved;
        }
        SerialType::Optional => {
            if f.idx > 0 {
                return Ok(());
            }
            // The Optional slot is `s.width` (= sizeof(RelPtr)) bytes. When
            // present, the inner T's data lives at the cursor and needs
            // `inner_align - 1` worst-case padding + sizeof(inner) +
            // whatever the inner's own variable extras contribute.
            let inner_schema = &s.parameters[0];
            self.add(s.width, f.x);
            if !self.reader.is_empty() && self.reader[0] == 0xc0 {
                rmp::decode::read_nil(&mut self.reader).ok();
            } else {
                let align = inner_schema.alignment().max(1);
                self.add(align - 1, f.x);
                self.child(st, &f, 0, inner_schema, f.x)?;
            }
        }
        SerialType::Table => {
            // Tables are not msgpack-sized; the calc-size helper is part
            // of the msgpack decode pipeline and would only be invoked on
            // a Table by mistake. Return a serialisation error rather
            // than an arbitrary number.
            return Err(MorlocError::Serialization(
                "Cannot compute msgpack size for a Table; Tables use the Arrow IPC SHM wire path".into(),
            ));
        }
        SerialType::Recur => unreachable!("a back-reference resolves before it is stepped"),
        }
        Ok(())
    }
}

impl<'r, 'd> SizeWalk<'r, 'd> {
    /// Size the `n` elements of an array whose header has been read. The
    /// count lives in the frame's `idx` high half while the low half is
    /// the next element, since the stream cannot be re-read on resume.
    fn elements(&mut self, st: &mut Stack<bool>, f: Frame<bool>, n: usize) -> Result<(), MorlocError> {
        let s: &'r Schema = self.res.resolve(unsafe { &*f.schema })?;
        let elem_schema = &s.parameters[0];
        let flat_elem = self.res.flat(elem_schema);
        for i in f.idx..n {
            if flat_elem {
                self.step(st, Frame::new(elem_schema, std::ptr::null(), f.x))?;
            } else {
                // The resume frame must know `n`: it is carried in `data`,
                // which this walker has no other use for.
                let mut g = f;
                g.data = n as *const u8;
                walk::defer(self, st, &g, i, elem_schema, std::ptr::null(), f.x);
                return Ok(());
            }
        }
        Ok(())
    }
}

fn skip_int(reader: &mut &[u8]) -> Result<(), MorlocError> {
    let marker = rmp::decode::read_marker(reader)
        .map_err(|_| MorlocError::Serialization("skip int: unexpected EOF".into()))?;
    let skip = match marker {
        rmp::Marker::FixPos(_) | rmp::Marker::FixNeg(_) => 0,
        rmp::Marker::U8 | rmp::Marker::I8 => 1,
        rmp::Marker::U16 | rmp::Marker::I16 => 2,
        rmp::Marker::U32 | rmp::Marker::I32 | rmp::Marker::F32 => 4,
        rmp::Marker::U64 | rmp::Marker::I64 | rmp::Marker::F64 => 8,
        _ => 0,
    };
    if reader.len() >= skip {
        *reader = &reader[skip..];
    }
    Ok(())
}

/// A tuple or record arrives as a msgpack array whose length must be the
/// field count; any other length would leave the stream misaligned and
/// every later field read as garbage.
fn check_field_count(schema: &Schema, n: usize) -> Result<(), MorlocError> {
    if n != schema.parameters.len() {
        return Err(MorlocError::Serialization(format!(
            "msgpack {} has {} elements but the type has {} fields",
            if schema.serial_type == SerialType::Map { "record" } else { "tuple" },
            n,
            schema.parameters.len()
        )));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::schema::parse_schema;
    use crate::json;

    #[test]
    fn test_roundtrip_nested_empty_string_via_msgpack() {
        let _shm = setup_shm();
        // An empty string inside a container has no data block; the packer
        // must write a zero-length string rather than follow the null.
        let schema = parse_schema("as").unwrap();
        let ptr1 = json::read_json_with_schema("[\"a\",\"\",\"ccc\"]", &schema).unwrap();
        let mpk = pack_with_schema(ptr1, &schema).unwrap();
        let ptr2 = unpack_with_schema(&mpk, &schema).unwrap();
        assert_eq!(json::voidstar_to_json_string(ptr2, &schema).unwrap(), "[\"a\",\"\",\"ccc\"]");
    }

    #[test]
    fn test_wrong_arity_tuple_is_rejected() {
        let _shm = setup_shm();
        // Three elements on the wire for a two-field tuple.
        let wire = parse_schema("t3i4i4i4").unwrap();
        let ptr = json::read_json_with_schema("[1,2,3]", &wire).unwrap();
        let mpk = pack_with_schema(ptr, &wire).unwrap();
        let two = parse_schema("t2i4i4").unwrap();
        let err = unpack_with_schema(&mpk, &two).unwrap_err().to_string();
        assert!(err.contains("3 elements but the type has 2 fields"), "{err}");
    }

    #[must_use]
    fn setup_shm() -> std::sync::RwLockReadGuard<'static, ()> {
        crate::init_test_shm()
    }

    #[test]
    fn test_roundtrip_int_via_msgpack() {
        let _shm = setup_shm();
        let schema = parse_schema("i4").unwrap();
        // JSON -> voidstar -> msgpack -> voidstar -> JSON
        let ptr1 = json::read_json_with_schema("42", &schema).unwrap();
        let mpk = pack_with_schema(ptr1, &schema).unwrap();
        let ptr2 = unpack_with_schema(&mpk, &schema).unwrap();
        let json_out = json::voidstar_to_json_string(ptr2, &schema).unwrap();
        assert_eq!(json_out, "42");
    }

    #[test]
    fn test_roundtrip_string_via_msgpack() {
        let _shm = setup_shm();
        let schema = parse_schema("s").unwrap();
        let ptr1 = json::read_json_with_schema("\"hello world\"", &schema).unwrap();
        let mpk = pack_with_schema(ptr1, &schema).unwrap();
        let ptr2 = unpack_with_schema(&mpk, &schema).unwrap();
        let json_out = json::voidstar_to_json_string(ptr2, &schema).unwrap();
        assert_eq!(json_out, "\"hello world\"");
    }

    #[test]
    fn test_roundtrip_array_via_msgpack() {
        let _shm = setup_shm();
        let schema = parse_schema("ai4").unwrap();
        let ptr1 = json::read_json_with_schema("[10,20,30]", &schema).unwrap();
        let mpk = pack_with_schema(ptr1, &schema).unwrap();
        let ptr2 = unpack_with_schema(&mpk, &schema).unwrap();
        let json_out = json::voidstar_to_json_string(ptr2, &schema).unwrap();
        assert_eq!(json_out, "[10,20,30]");
    }

    #[test]
    fn test_roundtrip_bool_via_msgpack() {
        let _shm = setup_shm();
        let schema = parse_schema("b").unwrap();
        let ptr1 = json::read_json_with_schema("true", &schema).unwrap();
        let mpk = pack_with_schema(ptr1, &schema).unwrap();
        let ptr2 = unpack_with_schema(&mpk, &schema).unwrap();
        let json_out = json::voidstar_to_json_string(ptr2, &schema).unwrap();
        assert_eq!(json_out, "true");
    }

    #[test]
    fn test_roundtrip_optional_null_via_msgpack() {
        let _shm = setup_shm();
        let schema = parse_schema("?i4").unwrap();
        let ptr1 = json::read_json_with_schema("null", &schema).unwrap();
        let mpk = pack_with_schema(ptr1, &schema).unwrap();
        let ptr2 = unpack_with_schema(&mpk, &schema).unwrap();
        let json_out = json::voidstar_to_json_string(ptr2, &schema).unwrap();
        assert_eq!(json_out, "null");
    }

    #[test]
    fn test_pack_only_string() {
        let _shm = setup_shm();
        let schema = parse_schema("s").unwrap();
        let ptr1 = json::read_json_with_schema("\"hi\"", &schema).unwrap();
        let mpk = pack_with_schema(ptr1, &schema).unwrap();
        assert!(!mpk.is_empty());
        assert_eq!(mpk.len(), 3);
    }

    #[test]
    fn test_unpack_only_int() {
        let _shm = setup_shm();
        let schema = parse_schema("i4").unwrap();
        // msgpack for 42 = [42] (fixint)
        let mpk = vec![42u8];
        let ptr = unpack_with_schema(&mpk, &schema).unwrap();
        let json_out = json::voidstar_to_json_string(ptr, &schema).unwrap();
        assert_eq!(json_out, "42");
    }

    #[test]
    fn test_unpack_only_string() {
        let _shm = setup_shm();
        let schema = parse_schema("s").unwrap();
        // msgpack for "hi" = [0xa2, 0x68, 0x69]
        let mpk = vec![0xa2, 0x68, 0x69];
        let size = calc_unpack_size(&mpk, &schema).unwrap();
        eprintln!("unpack size for string: {} (Array={}, total={})", size, std::mem::size_of::<shm::Array>(), size);
        let ptr = unpack_with_schema(&mpk, &schema).unwrap();
        let json_out = json::voidstar_to_json_string(ptr, &schema).unwrap();
        assert_eq!(json_out, "\"hi\"");
    }
}
