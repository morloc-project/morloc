use crate::error::MorlocError;

/// Morloc serial type identifiers, matching the C enum morloc_serial_type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum SerialType {
    Nil = 0,
    Bool = 1,
    Sint8 = 2,
    Sint16 = 3,
    Sint32 = 4,
    Sint64 = 5,
    Uint8 = 6,
    Uint16 = 7,
    Uint32 = 8,
    Uint64 = 9,
    Float32 = 10,
    Float64 = 11,
    String = 13,
    Array = 14,
    Tuple = 15,
    Map = 16,
    Optional = 17,
    Int = 18,       // variable-width integer (Array of uint64_t limbs, two's complement)
    Table = 19,     // Arrow IPC primitive. Schema entries (if any) are open
                    // constraints on the buffer's actual schema; the binary
                    // layout is fully described by the Arrow buffer itself.
    Recur = 20,     // Back-reference to a named schema declared by `&<klen><name>`
                    // earlier on the path. Carries the referenced name in the
                    // Schema's `name` field. Recursive records (e.g. Tree with a
                    // `[Tree]` field) terminate descent here; consumers resolve
                    // the cycle by walking up to the matching named declaration.
    IFile = 21,     // Random-access stream-file handle (read-only).
    OStream = 22,   // Sequential stream-file writer handle.
    IStream = 23,   // Sequential stream-file reader handle.
    // The three stream-handle variants share one wire layout: a 16-byte
    // tagged union.
    //   bytes 0..8  : tag in low byte; remaining bits reserved (zero).
    //   bytes 8..16 : payload (u64).
    // Tag values:
    //   0 = path-form: payload is a relptr to an Array<u8> carrying the
    //       file path. Receiver opens locally via `mlc_open(path, kind)`
    //       and binds a fresh slot in its own registry.
    //   1 = handle-form: payload is a bare `uint64_t` slot id meaningful
    //       in the receiver's shared SHM registry (intra-nexus only).
    //   2+ : reserved (future: content hash, URI, inline blob, ...).
    // `kind` for the receiver's open call is determined by the schema
    // code (`F` -> IFILE, `O` -> OSTREAM, `I` -> ISTREAM).
    Enum = 25,      // A `data` type whose constructors take no arguments. One
                    // byte: the constructor's 0-based position in the
                    // declaration, which is the wire tag. Constructor names
                    // travel in `keys` so JSON can render the name rather than
                    // the ordinal, `--json-help` can advertise a closed set,
                    // and an out-of-range tag can be rejected by name.
                    //
                    // Slot 12 is deliberately NOT reused here: it once held
                    // MORLOC_TENSOR, and an old packet carrying a 12 would be
                    // silently reinterpreted rather than rejected.
                    //
                    // Declaration order is part of the type's wire contract:
                    // appending a constructor keeps every existing value
                    // byte-identical, reordering does not.
}

/// Schema character codes for parsing schema strings.
const SCHEMA_NIL: u8 = b'z';
const SCHEMA_BOOL: u8 = b'b';
const SCHEMA_SINT: u8 = b'i';
const SCHEMA_UINT: u8 = b'u';
const SCHEMA_FLOAT: u8 = b'f';
const SCHEMA_STRING: u8 = b's';
const SCHEMA_ARRAY: u8 = b'a';
const SCHEMA_TUPLE: u8 = b't';
const SCHEMA_MAP: u8 = b'm';
const SCHEMA_OPTIONAL: u8 = b'?';
const SCHEMA_INT: u8 = b'j';
const SCHEMA_TABLE: u8 = b'T';
const SCHEMA_IFILE: u8 = b'F';
const SCHEMA_OSTREAM: u8 = b'O';
const SCHEMA_ISTREAM: u8 = b'I';
const SCHEMA_ENUM: u8 = b'e';

/// Recursive schema definition, mirroring the C Schema struct.
#[derive(Debug, Clone)]
pub struct Schema {
    pub serial_type: SerialType,
    /// Number of parameters (e.g., array has 1, tuple has N).
    pub size: usize,
    /// Byte width when stored in a fixed-width array.
    pub width: usize,
    /// Field offsets for tuples/records.
    pub offsets: Vec<usize>,
    /// Optional type hint string.
    pub hint: Option<String>,
    /// Child schemas (element type for arrays, field types for tuples, etc.).
    pub parameters: Vec<Schema>,
    /// Field names for records (None for non-record types).
    pub keys: Vec<String>,
    /// Named-schema declaration / back-reference name.
    ///
    /// Set in two cases by `parse_schema`:
    ///   * On the outer schema of a `&<klen><name>X` declaration, carrying
    ///     the declared name. The body itself is otherwise a normal schema.
    ///   * On every `Recur` node, carrying the back-referenced name. The
    ///     name was previously declared on an enclosing schema.
    ///
    /// All non-recursive schemas leave this `None`; round-trip preservation
    /// requires the renderer to emit `&<klen><n>` only when the carrying
    /// schema is the declaration site (i.e., the named schema's outer
    /// node), and to emit `^<klen><n>` for `Recur` nodes.
    pub name: Option<String>,
}

impl Schema {
    pub fn primitive(serial_type: SerialType) -> Self {
        use crate::shm_types as shm;
        let width = match serial_type {
            SerialType::Nil => 0,
            SerialType::Bool | SerialType::Sint8 | SerialType::Uint8 | SerialType::Enum => 1,
            SerialType::Sint16 | SerialType::Uint16 => 2,
            SerialType::Sint32 | SerialType::Uint32 | SerialType::Float32 => 4,
            SerialType::Sint64 | SerialType::Uint64 | SerialType::Float64 => 8,
            SerialType::String | SerialType::Int | SerialType::IFile
            | SerialType::OStream | SerialType::IStream
                => std::mem::size_of::<shm::Array>(),
            _ => 0,
        };
        Schema {
            serial_type,
            size: 0,
            width,
            offsets: Vec::new(),
            hint: None,
            parameters: Vec::new(),
            keys: Vec::new(),
            name: None,
        }
    }

    /// Returns true if this type has a fixed byte width (no variable-length data).
    pub fn is_fixed_width(&self) -> bool {
        match self.serial_type {
            SerialType::Nil
            | SerialType::Bool
            | SerialType::Sint8
            | SerialType::Sint16
            | SerialType::Sint32
            | SerialType::Sint64
            | SerialType::Uint8
            | SerialType::Uint16
            | SerialType::Uint32
            | SerialType::Uint64
            | SerialType::Float32
            | SerialType::Float64 => true,
            // A record is laid out exactly as the tuple of its field types:
            // both go through `calculate_tuple_layout`, and the field names
            // live in the schema rather than the buffer. So the same rule
            // applies -- fixed-width when every field is, which is what lets
            // an array of them be bulk-copied instead of walked.
            //
            // Answering true here means the bytes BETWEEN fields are read as
            // part of the value: bulk copy moves them, and the hash path
            // folds them in. That is sound only because free memory is
            // zeroed -- a fresh volume from the OS, and `shm::shfree`
            // scrubbing a block before republishing it -- so alignment
            // padding reads as zero rather than as whatever the block last
            // held. Were that scrub ever dropped, a hash over a padded
            // record or tuple would stop being reproducible.
            SerialType::Tuple | SerialType::Map => {
                self.parameters.iter().all(|p| p.is_fixed_width())
            }
            // One byte, no payload: fixed-width, so `[Enum]` takes the flat
            // bulk-copy path rather than a per-element walk.
            SerialType::Enum => true,
            SerialType::Optional => false,
            // A Recur back-references a record whose layout includes
            // variable-length payload; never fixed-width.
            SerialType::Recur => false,
            _ => false,
        }
    }

    /// True when this String/Array schema's data region is one contiguous flat
    /// blob (`arr.size * elem_width` bytes, no sub-allocations), so it can be
    /// bulk sized/copied/hashed instead of walked element by element. A
    /// `String`'s bytes are always flat; an `Array`'s data is flat exactly when
    /// its element is fixed-width.
    ///
    /// This is the element-level question the Array walks need, and is NOT
    /// `is_fixed_width()`: that asks whether the WHOLE value has a
    /// compile-time-fixed byte width, which is always false for a
    /// variable-length Array. Testing `is_fixed_width()` on an Array node
    /// compiles and runs but silently forces the O(bytes) per-element path --
    /// use this predicate for the bulk-vs-walk decision.
    pub fn array_data_is_flat(&self) -> bool {
        match self.serial_type {
            SerialType::String => true,
            SerialType::Array => self.parameters.first().map_or(false, |e| e.is_fixed_width()),
            _ => false,
        }
    }

    /// Alignment requirement for this type.
    pub fn alignment(&self) -> usize {
        match self.serial_type {
            SerialType::Nil => 1,
            // An enum is a single tag byte, so it aligns like a u8. This is
            // what lets it pack into an array with no padding.
            SerialType::Bool | SerialType::Sint8 | SerialType::Uint8 | SerialType::Enum => 1,
            SerialType::Sint16 | SerialType::Uint16 => 2,
            SerialType::Sint32 | SerialType::Uint32 | SerialType::Float32 => 4,
            SerialType::Sint64 | SerialType::Uint64 | SerialType::Float64 => 8,
            SerialType::String | SerialType::Array | SerialType::Map
            | SerialType::Int | SerialType::Table | SerialType::IFile
            | SerialType::OStream | SerialType::IStream => {
                // Table values live in SHM as a single relative pointer
                // to an Arrow buffer; same pointer-sized alignment as
                // other indirect types. F/O/I are wire-shaped as a 16-byte
                // tagged union (tag + payload) sharing the same
                // pointer-sized alignment.
                std::mem::size_of::<usize>() // pointer-sized alignment
            }
            // A back-ref ultimately resolves to a Map, which is pointer-aligned.
            SerialType::Recur => std::mem::size_of::<usize>(),
            SerialType::Tuple => {
                self.parameters
                    .iter()
                    .map(|p| p.alignment())
                    .max()
                    .unwrap_or(1)
            }
            SerialType::Optional => {
                // Pointer-aligned because the slot is now a relptr.
                std::mem::size_of::<usize>()
            }
        }
    }

    /// True if this schema is a primitive numeric type. Used to decide whether
    /// Array data buffers should be SIMD/BLAS-aligned.
    pub fn is_primitive_numeric(&self) -> bool {
        matches!(
            self.serial_type,
            SerialType::Sint8 | SerialType::Sint16 | SerialType::Sint32 | SerialType::Sint64
                | SerialType::Uint8 | SerialType::Uint16 | SerialType::Uint32 | SerialType::Uint64
                | SerialType::Float32 | SerialType::Float64
        )
    }

    /// Alignment for an Array's element data buffer in SHM. For primitive
    /// numerics, bumped to MORLOC_ARRAY_DATA_ALIGN (64 bytes -- SIMD/BLAS); for
    /// other element types, the natural alignment. Fixed constant in the wire
    /// format spec, architecture-independent.
    pub fn array_data_alignment(&self) -> usize {
        const MORLOC_ARRAY_DATA_ALIGN: usize = 64;
        let natural = self.alignment();
        if self.is_primitive_numeric() {
            std::cmp::max(MORLOC_ARRAY_DATA_ALIGN, natural)
        } else {
            natural
        }
    }
}

/// Parse a schema string into a Schema tree.
///
/// Positional format (no parentheses/commas):
/// - `z` -> Nil, `b` -> Bool, `s` -> String
/// - `i4` -> Sint32, `u8` -> Uint64, `f8` -> Float64
/// - `ai4` -> Array of Sint32
/// - `t2i4s` -> Tuple of (Sint32, String)
/// - `m24namesi4` -> Map with keys "name"->String, "i4"  (base-62 field count, then key-len + key + value for each)
/// - `?i4` -> Optional Sint32
/// - `<hint>i4` -> Sint32 with hint annotation
pub fn parse_schema(input: &str) -> Result<Schema, MorlocError> {
    let bytes = input.as_bytes();
    let mut declared: std::collections::HashSet<String> = std::collections::HashSet::new();
    let (schema, consumed) = parse_schema_r(bytes, 0, &mut declared)?;
    if consumed != bytes.len() {
        return Err(MorlocError::Schema(format!(
            "trailing characters after schema at position {consumed}"
        )));
    }
    Ok(schema)
}

/// Recursive schema parser matching the C `parse_schema_r` format exactly.
///
/// `declared` carries the set of names declared via `&<klen><name>` up to
/// this point in the walk. The set is threaded by mutable reference because
/// parsing is depth-first and a back-reference must see every declaration
/// from its enclosing path. Names go in when their declaration is
/// encountered; nothing is removed (the wire form does not nest scopes).
fn parse_schema_r(
    bytes: &[u8],
    pos: usize,
    declared: &mut std::collections::HashSet<String>,
) -> Result<(Schema, usize), MorlocError> {
    if pos >= bytes.len() {
        return Err(MorlocError::Schema("unexpected end of schema".into()));
    }

    let c = bytes[pos];
    let mut cur = pos + 1;

    match c {
        b'<' => {
            // Hint: <...> with nesting support, then parse the actual type
            let (hint, after_hint) = parse_hint(bytes, cur)?;
            let (mut schema, end) = parse_schema_r(bytes, after_hint, declared)?;
            schema.hint = Some(hint);
            Ok((schema, end))
        }
        b'&' => {
            // Named-schema declaration: `&<klen><name>X`. Reads the name,
            // marks it as declared, then parses the body and tags the body's
            // outer schema with the name. The body itself may contain
            // `^<klen><name>` back-references to the same name.
            //
            // After parsing, every Recur(name) node inside the body is
            // patched so its `width` matches the declaration's width. The
            // placeholder width set in `make_recur_schema` is correct for
            // schema-tree shape but wrong for runtime layout: the wire
            // form lays out an `Array<Recur(T)>` element as a full
            // `T`-shaped record, so iterators must step by the named
            // schema's width, not by a fixed pointer size.
            let (name, after_name) = parse_named_key(bytes, cur)?;
            declared.insert(name.clone());
            let (mut body, end) = parse_schema_r(bytes, after_name, declared)?;
            // Patch Recur widths and re-flow Tuple/Map widths bottom-up.
            // A Tuple/Map enclosing a Recur (e.g. `[(Str, X)]`) computed its
            // width using the placeholder Recur width; after patching, those
            // widths must be recalculated or Array iteration strides over
            // partial elements and corrupts inner relptr fields. Fixed-point
            // iterate: well-formed schemas (Recur behind Array/Optional or
            // directly as a Map field guarded by `classifyRecursion`) converge
            // in one round.
            for _ in 0..16 {
                let prev = body.width;
                patch_recur_widths_in(&mut body, &name, prev);
                recalculate_container_widths(&mut body);
                if body.width == prev { break; }
            }
            body.name = Some(name);
            Ok((body, end))
        }
        b'^' => {
            // Back-reference: `^<klen><name>`. The name must have been
            // declared by an enclosing `&<klen><name>` on this walk; a
            // dangling back-ref is a clean schema error.
            let (name, after_name) = parse_named_key(bytes, cur)?;
            if !declared.contains(&name) {
                return Err(MorlocError::Schema(format!(
                    "back-reference to undeclared name '{name}'"
                )));
            }
            Ok((make_recur_schema(name), after_name))
        }
        SCHEMA_NIL => Ok((Schema::primitive(SerialType::Nil), cur)),
        SCHEMA_BOOL => Ok((Schema::primitive(SerialType::Bool), cur)),
        SCHEMA_STRING => {
            // String schema has one parameter (uint8) for array compatibility,
            // matching the C string_schema() constructor.
            Ok((Schema {
                serial_type: SerialType::String,
                size: 1,
                width: std::mem::size_of::<crate::shm_types::Array>(),
                offsets: Vec::new(),
                hint: None,
                parameters: vec![Schema::primitive(SerialType::Uint8)],
                keys: Vec::new(),
                name: None,
            }, cur))
        }
        SCHEMA_IFILE | SCHEMA_OSTREAM | SCHEMA_ISTREAM => {
            // F/O/I share one wire layout: a 16-byte tagged union (tag in
            // low byte of the first 8 bytes, payload u64 in the second 8).
            // `parameters[0]` carries `Uint8` so walkers that descend into
            // String-shaped fields keep working without special-casing the
            // outer schema.
            let st = match c {
                SCHEMA_IFILE => SerialType::IFile,
                SCHEMA_OSTREAM => SerialType::OStream,
                _ => SerialType::IStream,
            };
            Ok((Schema {
                serial_type: st,
                size: 1,
                width: std::mem::size_of::<crate::shm_types::Array>(),
                offsets: Vec::new(),
                hint: None,
                parameters: vec![Schema::primitive(SerialType::Uint8)],
                keys: Vec::new(),
                name: None,
            }, cur))
        }
        SCHEMA_SINT => parse_sized_int(bytes, cur, true),
        SCHEMA_UINT => parse_sized_int(bytes, cur, false),
        SCHEMA_FLOAT => parse_sized_float(bytes, cur),
        SCHEMA_ARRAY => {
            // Array: optional dimension constraint (:N in decimal), then child schema
            let expected_len = if cur < bytes.len() && bytes[cur] == b':' {
                cur += 1;
                let (n, after) = parse_decimal(bytes, cur)?;
                cur = after;
                n
            } else {
                0 // unconstrained
            };
            let (child, end) = parse_schema_r(bytes, cur, declared)?;
            Ok((make_array_schema_with_dim(expected_len, child), end))
        }
        SCHEMA_INT => {
            // Variable-width integer: no parameters, uses Array layout
            Ok((Schema::primitive(SerialType::Int), cur))
        }
        SCHEMA_OPTIONAL => {
            // Optional: one child schema follows immediately
            let (child, end) = parse_schema_r(bytes, cur, declared)?;
            Ok((make_optional_schema(child), end))
        }
        SCHEMA_TUPLE => {
            // Tuple: base-62 size char, then N child schemas
            if cur >= bytes.len() {
                return Err(MorlocError::Schema("expected tuple size".into()));
            }
            let (n, mut p) = read_count(bytes, cur)?;
            let mut params = Vec::with_capacity(n);
            for _ in 0..n {
                let (child, end) = parse_schema_r(bytes, p, declared)?;
                params.push(child);
                p = end;
            }
            Ok((make_tuple_schema(params), p))
        }
        SCHEMA_MAP => {
            // Map/record: base-62 size char, then N (key_len_char + key_bytes + value_schema)
            if cur >= bytes.len() {
                return Err(MorlocError::Schema("expected map size".into()));
            }
            let (n, mut p) = read_count(bytes, cur)?;
            let mut params = Vec::with_capacity(n);
            let mut keys = Vec::with_capacity(n);
            for _ in 0..n {
                // Read key: length (base-62, escaped past 63) + that many bytes
                if p >= bytes.len() {
                    return Err(MorlocError::Schema("expected map key length".into()));
                }
                let (key_len, kp) = read_count(bytes, p)?;
                p = kp;
                if p + key_len > bytes.len() {
                    return Err(MorlocError::Schema("map key extends past end".into()));
                }
                let key = std::str::from_utf8(&bytes[p..p + key_len])
                    .map_err(|_| MorlocError::Schema("invalid UTF-8 in map key".into()))?
                    .to_string();
                p += key_len;
                keys.push(key);
                // Read value schema
                let (child, end) = parse_schema_r(bytes, p, declared)?;
                params.push(child);
                p = end;
            }
            Ok((make_map_schema(params, keys), p))
        }
        SCHEMA_ENUM => {
            // Enum: count, then N (key_len + constructor name). No child
            // schemas -- an argument-free constructor carries no payload.
            let (n, mut p) = read_count(bytes, cur)?;
            let mut keys = Vec::with_capacity(n);
            for _ in 0..n {
                if p >= bytes.len() {
                    return Err(MorlocError::Schema("expected enum constructor length".into()));
                }
                let (klen, kp) = read_count(bytes, p)?;
                p = kp;
                if p + klen > bytes.len() {
                    return Err(MorlocError::Schema(
                        "enum constructor name extends past end".into(),
                    ));
                }
                let key = std::str::from_utf8(&bytes[p..p + klen])
                    .map_err(|_| {
                        MorlocError::Schema("invalid UTF-8 in enum constructor name".into())
                    })?
                    .to_string();
                p += klen;
                keys.push(key);
            }
            if keys.len() > 256 {
                return Err(MorlocError::Schema(format!(
                    "enum has {} constructors; the limit is 256 so a tag fits in one byte",
                    keys.len()
                )));
            }
            Ok((make_enum_schema(keys), p))
        }
        SCHEMA_TABLE => {
            // Table primitive (Arrow IPC).
            //
            // Two surface forms:
            //   `T`           -- bare token; no declared columns. The
            //                   buffer's Arrow schema is opaque to morloc;
            //                   any value is accepted.
            //   `T:K<entries>` -- K declared columns of declared types,
            //                    parsed identically to `m`'s entries
            //                    (one base-62 length char + key bytes
            //                    + child schema). Open semantics: these
            //                    are *minimum* constraints -- the buffer
            //                    may carry extra columns.
            //
            // The colon disambiguates bare `T` from `T:0` (zero declared
            // columns). Bare `T` means "schema unspecified"; `T:0` means
            // "exactly zero columns required" (rare but legal).
            if cur < bytes.len() && bytes[cur] == b':' {
                cur += 1;
                if cur >= bytes.len() {
                    return Err(MorlocError::Schema("expected table column count after ':'".into()));
                }
                let (n, mut p) = read_count(bytes, cur)?;
                let mut params = Vec::with_capacity(n);
                let mut keys = Vec::with_capacity(n);
                for _ in 0..n {
                    if p >= bytes.len() {
                        return Err(MorlocError::Schema("expected table column key length".into()));
                    }
                    let (key_len, kp) = read_count(bytes, p)?;
                    p = kp;
                    if p + key_len > bytes.len() {
                        return Err(MorlocError::Schema("table column key extends past end".into()));
                    }
                    let key = std::str::from_utf8(&bytes[p..p + key_len])
                        .map_err(|_| MorlocError::Schema("invalid UTF-8 in table key".into()))?
                        .to_string();
                    p += key_len;
                    keys.push(key);
                    let (child, end) = parse_schema_r(bytes, p, declared)?;
                    params.push(child);
                    p = end;
                }
                Ok((make_table_schema(params, keys), p))
            } else {
                Ok((make_table_schema(Vec::new(), Vec::new()), cur))
            }
        }
        _ => Err(MorlocError::Schema(format!(
            "unknown schema character '{}' at position {pos}",
            c as char
        ))),
    }
}

/// Parse hint with nested angle bracket support: `<std::vector<$1>>` etc.
/// Parse a `<klen><name>` length-prefixed identifier, the same syntax used
/// for record keys but in the slots after `&` and `^` markers. Returns the
/// name string and the position just past its last byte.
fn parse_named_key(bytes: &[u8], pos: usize) -> Result<(String, usize), MorlocError> {
    if pos >= bytes.len() {
        return Err(MorlocError::Schema("expected name length".into()));
    }
    let (klen, start) = read_count(bytes, pos)?;
    let end = start + klen;
    if end > bytes.len() {
        return Err(MorlocError::Schema("schema name extends past end".into()));
    }
    let name = std::str::from_utf8(&bytes[start..end])
        .map_err(|_| MorlocError::Schema("invalid UTF-8 in schema name".into()))?
        .to_string();
    Ok((name, end))
}

fn parse_hint(bytes: &[u8], pos: usize) -> Result<(String, usize), MorlocError> {
    let mut depth: usize = 1;
    let start = pos;
    let mut cur = pos;
    while cur < bytes.len() {
        match bytes[cur] {
            b'<' => depth += 1,
            b'>' => {
                depth -= 1;
                if depth == 0 {
                    let hint = std::str::from_utf8(&bytes[start..cur])
                        .unwrap_or("")
                        .to_string();
                    return Ok((hint, cur + 1)); // skip closing '>'
                }
            }
            _ => {}
        }
        cur += 1;
    }
    Err(MorlocError::Schema("unclosed '<' in hint".into()))
}

fn parse_sized_int(
    bytes: &[u8],
    pos: usize,
    signed: bool,
) -> Result<(Schema, usize), MorlocError> {
    if pos >= bytes.len() {
        return Err(MorlocError::Schema("expected size after 'i'/'u'".into()));
    }
    // Size is a SINGLE base-62 character, not a multi-digit number
    let size = decode_base62(bytes[pos])?;
    let next = pos + 1;
    let st = match (signed, size) {
        (true, 1) => SerialType::Sint8,
        (true, 2) => SerialType::Sint16,
        (true, 4) => SerialType::Sint32,
        (true, 8) => SerialType::Sint64,
        (false, 1) => SerialType::Uint8,
        (false, 2) => SerialType::Uint16,
        (false, 4) => SerialType::Uint32,
        (false, 8) => SerialType::Uint64,
        _ => return Err(MorlocError::Schema(format!("invalid integer size {size}"))),
    };
    Ok((Schema::primitive(st), next))
}

fn parse_sized_float(bytes: &[u8], pos: usize) -> Result<(Schema, usize), MorlocError> {
    if pos >= bytes.len() {
        return Err(MorlocError::Schema("expected size after 'f'".into()));
    }
    // Size is a SINGLE base-62 character, not a multi-digit number
    let size = decode_base62(bytes[pos])?;
    let next = pos + 1;
    let st = match size {
        4 => SerialType::Float32,
        8 => SerialType::Float64,
        _ => return Err(MorlocError::Schema(format!("invalid float size {size}"))),
    };
    Ok((Schema::primitive(st), next))
}

/// Decode a single base-62 character to a number (0-63).
/// 0-9 -> 0-9, a-z -> 10-35, A-Z -> 36-61, + -> 62, / -> 63
fn decode_base62(c: u8) -> Result<usize, MorlocError> {
    match c {
        b'0'..=b'9' => Ok((c - b'0') as usize),
        b'a'..=b'z' => Ok((c - b'a') as usize + 10),
        b'A'..=b'Z' => Ok((c - b'A') as usize + 36),
        b'+' => Ok(62),
        b'/' => Ok(63),
        _ => Err(MorlocError::Schema(format!(
            "invalid base-62 size character '{}'",
            c as char
        ))),
    }
}

fn encode_base62(n: usize) -> char {
    match n {
        0..=9 => (b'0' + n as u8) as char,
        10..=35 => (b'a' + (n - 10) as u8) as char,
        36..=61 => (b'A' + (n - 36) as u8) as char,
        62 => '+',
        63 => '/',
        // Unreachable: every caller goes through `write_count`, which
        // splits a value of 64 or more into single-digit limbs before
        // reaching here. A bare digit cannot represent it.
        _ => unreachable!("base-62 digit out of range: {n}"),
    }
}

/// Read a count or key length, which the compiler encodes as one base-62
/// digit or, for values of 64 or more, as an escape.
///
/// `Morloc.CodeGenerator.Serial.encode64` is the other side:
///
/// ```text
/// encode64 i | i < 64    = <one base-62 digit>
///            | otherwise = '=' : encode64 (i `mod` 64) ++ encode64 (i `div` 64)
/// ```
///
/// so an escaped value is a little-endian base-64 numeral: a run of
/// `'=' <digit>` limbs followed by a bare final digit. Written iteratively
/// rather than recursively because the byte stream can come off the wire,
/// and a long run of `=` must not become a deep call stack.
///
/// Returns the value and the position just past the last byte consumed.
fn read_count(bytes: &[u8], pos: usize) -> Result<(usize, usize), MorlocError> {
    let mut limbs: Vec<usize> = Vec::new();
    let mut cur = pos;
    loop {
        if cur >= bytes.len() {
            return Err(MorlocError::Schema("expected a count".into()));
        }
        if bytes[cur] == b'=' {
            cur += 1;
            if cur >= bytes.len() {
                return Err(MorlocError::Schema(
                    "truncated escaped count: '=' with no digit".into(),
                ));
            }
            limbs.push(decode_base62(bytes[cur])?);
            cur += 1;
        } else {
            limbs.push(decode_base62(bytes[cur])?);
            cur += 1;
            break;
        }
    }
    let mut value: usize = 0;
    for limb in limbs.iter().rev() {
        value = value
            .checked_mul(64)
            .and_then(|v| v.checked_add(*limb))
            .ok_or_else(|| MorlocError::Schema("count overflows a usize".into()))?;
    }
    Ok((value, cur))
}

/// Write a count or key length, mirroring `read_count`.
fn write_count(buf: &mut String, n: usize) {
    let mut rest = n;
    while rest >= 64 {
        buf.push('=');
        buf.push(encode_base62(rest % 64));
        rest /= 64;
    }
    buf.push(encode_base62(rest));
}

/// Parse a decimal integer from the byte stream. Returns (value, position after last digit).
fn parse_decimal(bytes: &[u8], pos: usize) -> Result<(usize, usize), MorlocError> {
    let mut cur = pos;
    let mut n: usize = 0;
    if cur >= bytes.len() || !bytes[cur].is_ascii_digit() {
        return Err(MorlocError::Schema("expected decimal digit".into()));
    }
    while cur < bytes.len() && bytes[cur].is_ascii_digit() {
        n = n * 10 + (bytes[cur] - b'0') as usize;
        cur += 1;
    }
    Ok((n, cur))
}

// ── Schema constructors ────────────────────────────────────────────────────

fn make_array_schema_with_dim(expected_len: usize, child: Schema) -> Schema {
    Schema {
        serial_type: SerialType::Array,
        size: 1,
        width: std::mem::size_of::<crate::shm_types::Array>(),
        offsets: vec![expected_len],
        hint: None,
        parameters: vec![child],
        keys: Vec::new(),
        name: None,
    }
}

fn make_optional_schema(child: Schema) -> Schema {
    // Optional's voidstar slot is a single relative pointer: RELNULL
    // for absent, otherwise the pointer to T's body elsewhere in the
    // buffer. This is what makes Optional<Recur(T)> work -- the slot
    // width no longer depends on T.width, so the recursive width
    // equation has a finite fixed point.
    //
    // Trade-off: dense-mostly-present numeric Optionals carry one
    // indirection per element. The right primitive for that case is a
    // validity-bitmap-bearing column type (e.g. through Table), not
    // per-element Optional.
    Schema {
        serial_type: SerialType::Optional,
        size: 1,
        width: std::mem::size_of::<crate::shm_types::RelPtr>(),
        offsets: Vec::new(),
        hint: None,
        parameters: vec![child],
        keys: Vec::new(),
        name: None,
    }
}

fn make_tuple_schema(params: Vec<Schema>) -> Schema {
    let (width, offsets) = calculate_tuple_layout(&params);
    let size = params.len();
    Schema {
        serial_type: SerialType::Tuple,
        size,
        width,
        offsets,
        hint: None,
        parameters: params,
        keys: Vec::new(),
        name: None,
    }
}

fn make_map_schema(params: Vec<Schema>, keys: Vec<String>) -> Schema {
    let (width, offsets) = calculate_tuple_layout(&params);
    let size = params.len();
    Schema {
        serial_type: SerialType::Map,
        size,
        width,
        offsets,
        hint: None,
        parameters: params,
        keys,
        name: None,
    }
}

/// Construct a Table schema from its open column constraints.
///
/// The morloc Table primitive carries an Arrow IPC buffer whose binary
/// layout is fully self-describing; this Schema only records the
/// declared columns (if any) for runtime constraint checking and for
/// driving CSV/JSON parsing into typed Arrow fields. Empty `params` /
/// `keys` correspond to the bare `T` wire form -- no declared columns,
/// any Arrow buffer accepted. Non-empty entries are open constraints
/// (the buffer may carry additional columns beyond these).
///
/// Width is the size of the SHM relative pointer that owns the Arrow
/// buffer; the column entries themselves do not contribute to in-memory
/// layout because the data lives outside the schema-described region.
/// Construct an Enum schema from its constructor names.
///
/// One byte wide and fixed-width, which is the whole point of the form:
/// `array_data_is_flat` is then true for `[Enum]`, so an array of them is
/// bulk-copied and gets the 64-byte-aligned data buffer that primitive
/// numerics get. A `[DNA]` is byte-for-byte a `[U8]` in shared memory.
fn make_enum_schema(keys: Vec<String>) -> Schema {
    Schema {
        serial_type: SerialType::Enum,
        size: keys.len(),
        width: 1,
        offsets: Vec::new(),
        hint: None,
        parameters: Vec::new(),
        keys,
        name: None,
    }
}

fn make_table_schema(params: Vec<Schema>, keys: Vec<String>) -> Schema {
    Schema {
        serial_type: SerialType::Table,
        size: params.len(),
        width: std::mem::size_of::<crate::shm_types::Array>(),
        offsets: Vec::new(),
        hint: None,
        parameters: params,
        keys,
        name: None,
    }
}

/// Build a Recur back-reference Schema for the given declared name.
///
/// A Recur node is structurally minimal: it carries no parameters or
/// keys, only the back-referenced name. Width is set to a placeholder
/// here; `parse_schema` patches it to the resolved declaration's width
/// once the named schema's body is fully parsed (see
/// `patch_recur_widths_in`). The placeholder is pointer-sized so the
/// shape of intermediate width calculations stays sane if patching is
/// somehow skipped.
fn make_recur_schema(name: String) -> Schema {
    Schema {
        serial_type: SerialType::Recur,
        size: 0,
        width: std::mem::size_of::<crate::shm_types::Array>(),
        offsets: Vec::new(),
        hint: None,
        parameters: Vec::new(),
        keys: Vec::new(),
        name: Some(name),
    }
}

/// Walk a parsed schema sub-tree and patch every `Recur(name)` node's
/// `width` to the supplied value. Called once per `&<name>X`
/// declaration so the back-reference's runtime layout (used by Array
/// iteration, Optional indirection, and the C++ allocator) matches the
/// declaration's own width.
fn patch_recur_widths_in(schema: &mut Schema, name: &str, width: usize) {
    for p in &mut schema.parameters {
        if matches!(p.serial_type, SerialType::Recur) {
            if let Some(ref n) = p.name {
                if n == name {
                    p.width = width;
                }
            }
        }
        patch_recur_widths_in(p, name, width);
    }
}

/// Recompute Tuple and Map widths/offsets bottom-up from current parameter
/// widths. Used after `patch_recur_widths_in` to propagate the resolved
/// Recur width into enclosing Tuple/Map layouts. Array and Optional widths
/// are fixed and need no update; primitives have no children.
fn recalculate_container_widths(schema: &mut Schema) {
    for p in &mut schema.parameters {
        recalculate_container_widths(p);
    }
    if matches!(schema.serial_type, SerialType::Tuple | SerialType::Map) {
        let (w, o) = calculate_tuple_layout(&schema.parameters);
        schema.width = w;
        schema.offsets = o;
    }
}

/// Calculate byte offsets for tuple fields (C struct layout with natural alignment).
fn calculate_tuple_layout(params: &[Schema]) -> (usize, Vec<usize>) {
    let mut offsets = Vec::with_capacity(params.len());
    let mut offset: usize = 0;
    let mut max_align: usize = 1;

    for param in params {
        let align = param.alignment();
        max_align = std::cmp::max(max_align, align);
        // Align the offset
        offset = (offset + align - 1) & !(align - 1);
        offsets.push(offset);
        offset += param.width;
    }

    // Total width padded to max alignment
    let width = (offset + max_align - 1) & !(max_align - 1);
    (width, offsets)
}

/// Render a schema back to its string representation.
pub fn schema_to_string(schema: &Schema) -> String {
    let mut buf = String::new();
    schema_to_string_inner(schema, &mut buf);
    buf
}

/// Structural compatibility with a wildcard rule for Array length:
/// two Array schemas match if their lengths are equal or if either is
/// `0` (unconstrained). Everything else -- serial type, size, keys,
/// sub-schemas, declared/recur names -- must match exactly.
pub fn schemas_compatible(a: &Schema, b: &Schema) -> bool {
    if a.serial_type != b.serial_type
        || a.size != b.size
        || a.keys != b.keys
        || a.name != b.name
        || a.parameters.len() != b.parameters.len()
    {
        return false;
    }
    // Array length compatibility: zero is wildcard on either side.
    if a.serial_type == SerialType::Array {
        let la = a.offsets.first().copied().unwrap_or(0);
        let lb = b.offsets.first().copied().unwrap_or(0);
        if la != 0 && lb != 0 && la != lb {
            return false;
        }
    } else if a.offsets != b.offsets {
        return false;
    }
    a.parameters.iter().zip(b.parameters.iter())
        .all(|(pa, pb)| schemas_compatible(pa, pb))
}

/// Canonical form of a schema string: parsed, then re-rendered. This
/// drops the `<hint>` prefixes that a compiler-generated pool schema
/// carries, because `schema_to_string` does not emit them.
///
/// Every packet writer stores this form (`make_standard_data_packet`,
/// `make_mesg_data_packet`, `make_stream_header_block`), so it is the
/// only form that may be persisted or compared. An entry point that
/// takes a schema string from a caller normalizes with this rather than
/// trusting the caller to have done it: the nexus evaluator normalized
/// and the pools did not, which is how the two came to disagree at
/// `@append`.
///
/// An unparseable string is returned unchanged, so the caller's own
/// parse produces the diagnostic rather than this function inventing one.
pub fn canonicalize_schema_str(s: &str) -> String {
    match parse_schema(s) {
        Ok(parsed) => schema_to_string(&parsed),
        Err(_) => s.to_string(),
    }
}

/// String-form entry point for the wire-boundary comparator. Parses both
/// operands via `parse_schema` and structurally compares. Returns true iff
/// the two schemas describe compatible wire forms under the gradual-typing
/// subtype rule (see `schemas_compatible`).
pub fn schema_strings_compatible(stored: &str, requested: &str) -> bool {
    match (parse_schema(stored), parse_schema(requested)) {
        (Ok(a), Ok(b)) => schemas_compatible(&a, &b),
        // If either side fails to parse, fall back to strict string equality.
        // This preserves the previous behavior for malformed schemas rather
        // than silently accepting them as compatible.
        _ => stored == requested,
    }
}

fn schema_to_string_inner(schema: &Schema, buf: &mut String) {
    // `<hint>` prefixes are deliberately NOT emitted: hints are
    // compile-time state for pool-side native dispatch (numpy /
    // std::vector / list) and don't belong on the wire. Pools get
    // hints from parsing the compiler-generated schema strings baked
    // into pool.cpp / pymorloc, not from any wire round-trip. So
    // `schema_to_string(parse_schema(s))` is intentionally NOT
    // identity when `s` contains a hint.

    // Emit `&<klen><n>` for a named-schema declaration. Only the
    // declaration site carries `name` on a non-Recur node; `Recur`
    // nodes also carry `name` (the back-ref target) but are emitted as
    // `^<klen><n>` in their own arm below.
    if schema.serial_type != SerialType::Recur {
        if let Some(ref n) = schema.name {
            buf.push('&');
            write_count(buf, n.len());
            buf.push_str(n);
        }
    }

    match schema.serial_type {
        SerialType::Nil => buf.push('z'),
        SerialType::Bool => buf.push('b'),
        SerialType::Sint8 => buf.push_str("i1"),
        SerialType::Sint16 => buf.push_str("i2"),
        SerialType::Sint32 => buf.push_str("i4"),
        SerialType::Sint64 => buf.push_str("i8"),
        SerialType::Uint8 => buf.push_str("u1"),
        SerialType::Uint16 => buf.push_str("u2"),
        SerialType::Uint32 => buf.push_str("u4"),
        SerialType::Uint64 => buf.push_str("u8"),
        SerialType::Float32 => buf.push_str("f4"),
        SerialType::Float64 => buf.push_str("f8"),
        SerialType::String => buf.push('s'),
        SerialType::IFile => buf.push('F'),
        SerialType::OStream => buf.push('O'),
        SerialType::IStream => buf.push('I'),
        SerialType::Array => {
            buf.push('a');
            let expected = schema.offsets.first().copied().unwrap_or(0);
            if expected > 0 {
                buf.push(':');
                buf.push_str(&expected.to_string());
            }
            schema_to_string_inner(&schema.parameters[0], buf);
        }
        SerialType::Tuple => {
            buf.push('t');
            write_count(buf, schema.size);
            for p in &schema.parameters {
                schema_to_string_inner(p, buf);
            }
        }
        SerialType::Map => {
            buf.push('m');
            write_count(buf, schema.size);
            for (i, p) in schema.parameters.iter().enumerate() {
                if i < schema.keys.len() {
                    let key = &schema.keys[i];
                    write_count(buf, key.len());
                    buf.push_str(key);
                }
                schema_to_string_inner(p, buf);
            }
        }
        SerialType::Enum => {
            buf.push('e');
            write_count(buf, schema.size);
            for key in &schema.keys {
                write_count(buf, key.len());
                buf.push_str(key);
            }
        }
        SerialType::Int => {
            buf.push('j');
        }
        SerialType::Optional => {
            buf.push('?');
            schema_to_string_inner(&schema.parameters[0], buf);
        }
        SerialType::Table => {
            // Round-trip with the parser: bare `T` for empty constraint
            // list, `T:K<entries>` otherwise. Any parsed hint is
            // dropped on the way out (see the header comment on
            // schema_to_string_inner).
            buf.push('T');
            if schema.size > 0 {
                buf.push(':');
                write_count(buf, schema.size);
                for (i, p) in schema.parameters.iter().enumerate() {
                    if i < schema.keys.len() {
                        let key = &schema.keys[i];
                        write_count(buf, key.len());
                        buf.push_str(key);
                    }
                    schema_to_string_inner(p, buf);
                }
            }
        }
        SerialType::Recur => {
            // Back-reference: emit `^<klen><name>`. The referenced name is
            // guaranteed present by the parser (dangling refs are rejected).
            buf.push('^');
            if let Some(ref n) = schema.name {
                write_count(buf, n.len());
                buf.push_str(n);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_primitives() {
        assert_eq!(parse_schema("z").unwrap().serial_type, SerialType::Nil);
        assert_eq!(parse_schema("b").unwrap().serial_type, SerialType::Bool);
        assert_eq!(parse_schema("i4").unwrap().serial_type, SerialType::Sint32);
        assert_eq!(parse_schema("u8").unwrap().serial_type, SerialType::Uint64);
        assert_eq!(parse_schema("f8").unwrap().serial_type, SerialType::Float64);
        assert_eq!(parse_schema("s").unwrap().serial_type, SerialType::String);
    }

    #[test]
    fn test_parse_array() {
        let s = parse_schema("ai4").unwrap();
        assert_eq!(s.serial_type, SerialType::Array);
        assert_eq!(s.parameters.len(), 1);
        assert_eq!(s.parameters[0].serial_type, SerialType::Sint32);
    }

    #[test]
    fn test_parse_tuple() {
        let s = parse_schema("t3i4sf8").unwrap();
        assert_eq!(s.serial_type, SerialType::Tuple);
        assert_eq!(s.parameters.len(), 3);
    }

    #[test]
    fn test_parse_nested() {
        let s = parse_schema("at2i4s").unwrap();
        assert_eq!(s.serial_type, SerialType::Array);
        assert_eq!(s.parameters[0].serial_type, SerialType::Tuple);
        assert_eq!(s.parameters[0].parameters.len(), 2);
    }

    #[test]
    fn test_parse_map() {
        let s = parse_schema("m21as1bi4").unwrap();
        assert_eq!(s.serial_type, SerialType::Map);
        assert_eq!(s.parameters.len(), 2);
        assert_eq!(s.keys[0], "a");
        assert_eq!(s.keys[1], "b");
    }

    #[test]
    fn test_parse_optional() {
        let s = parse_schema("?f8").unwrap();
        assert_eq!(s.serial_type, SerialType::Optional);
        assert_eq!(s.parameters[0].serial_type, SerialType::Float64);
    }

    #[test]
    fn test_parse_with_hints() {
        let s = parse_schema("<float>f8").unwrap();
        assert_eq!(s.serial_type, SerialType::Float64);
        assert_eq!(s.hint.as_deref(), Some("float"));

        // Nested hints: <std::vector<$1>>
        let s = parse_schema("<std::vector<$1>>ai4").unwrap();
        assert_eq!(s.serial_type, SerialType::Array);
        assert_eq!(s.hint.as_deref(), Some("std::vector<$1>"));
    }

    // A pool's schema string carries the language's concrete form as a
    // `<hint>` prefix; every packet writer stores the hint-free form
    // (schema_to_string drops hints by design). The two describe one wire
    // type, so any comparison at a wire boundary must ignore the hint.
    #[test]
    fn canonicalize_drops_hints_and_is_idempotent() {
        assert_eq!(canonicalize_schema_str("a<dict>m24kinds2idj"), "am24kinds2idj");
        assert_eq!(canonicalize_schema_str("a<str>s"), "as");
        assert_eq!(canonicalize_schema_str("<std::vector<$1>>ai4"), "ai4");
        // already canonical
        assert_eq!(canonicalize_schema_str("am24kinds2idj"), "am24kinds2idj");
        // idempotent
        let once = canonicalize_schema_str("a<dict>m21x<int>j1yj");
        assert_eq!(canonicalize_schema_str(&once), once);
        // dims survive; they are part of the type, not a hint
        assert_eq!(canonicalize_schema_str("a:5j"), "a:5j");
        // unparseable input is handed back untouched for the caller to report
        assert_eq!(canonicalize_schema_str("not a schema"), "not a schema");
    }

    #[test]
    fn compatible_ignores_concrete_type_hints() {
        assert!(schema_strings_compatible("am24kinds2idj", "a<dict>m24kinds2idj"));
        assert!(schema_strings_compatible("a<dict>m24kinds2idj", "am24kinds2idj"));
        // Hints are not a record-only concern: `type Tag = Str` gets one too.
        assert!(schema_strings_compatible("as", "a<str>s"));
        // Nested hints (the C++ container forms) parse and are ignored.
        assert!(schema_strings_compatible("ai4", "<std::vector<$1>>ai4"));
        // A hint on an inner node, not just the outer one.
        assert!(schema_strings_compatible("am21xj1yj", "a<dict>m21x<int>j1yj"));
    }

    // Ignoring hints must not make the check vacuous.
    #[test]
    fn compatible_rejects_genuinely_different_types() {
        // different record keys
        assert!(!schema_strings_compatible("am24kinds2idj", "a<dict>m25alphas4betaj"));
        // different leaf type
        assert!(!schema_strings_compatible("aj", "as"));
        // different shape
        assert!(!schema_strings_compatible("aj", "j"));
        // tuple field order
        assert!(!schema_strings_compatible("t2js", "t2sj"));
        // different arity
        assert!(!schema_strings_compatible("t2js", "t3jss"));
    }

    // An unconstrained array length (0) is a wildcard against a
    // constrained one. This is deliberate: `make_array_schema_with_dim`
    // gives both the same width and `is_fixed_width` is false for every
    // Array, so the dim is a validation constraint (enforced on the JSON
    // ingestion path) rather than a layout difference.
    #[test]
    fn compatible_treats_array_length_zero_as_wildcard() {
        assert!(schema_strings_compatible("a:5j", "aj"));
        assert!(schema_strings_compatible("aj", "a:5j"));
        assert!(!schema_strings_compatible("a:5j", "a:6j"));
    }

    #[test]
    fn test_roundtrip() {
        let cases = ["z", "b", "i4", "u8", "f8", "s", "ai4", "t2i4s", "?i4"];
        for case in cases {
            let schema = parse_schema(case).unwrap();
            let rendered = schema_to_string(&schema);
            assert_eq!(rendered, case, "roundtrip failed for '{case}'");
        }
    }

    #[test]
    fn test_roundtrip_map() {
        let input = "m24names4infoi4";
        let schema = parse_schema(input).unwrap();
        assert_eq!(schema.serial_type, SerialType::Map);
        assert_eq!(schema.keys, vec!["name", "info"]);
        let rendered = schema_to_string(&schema);
        assert_eq!(rendered, input);
    }

    // Recursive schemas: a record that references itself through a list
    // or optional field. The wire form gets two new tokens,
    // `&<klen><name>` (declare) and `^<klen><name>` (back-ref). For a
    // recursive Tree {value::Int, children::[Tree]} the form reads
    //   &4Treem25valuej8childrena^4Tree
    //
    // The parser does NOT materialize the cycle: a `Recur` node carries
    // the back-referenced name and consumers walk up to the declaration
    // when they need the body. This keeps the structure finite (no
    // unbounded expansion at parse time) and round-trip preserves bytes
    // since each declaration appears exactly once.

    #[test]
    fn test_parse_recursive_tree_list() {
        let s = parse_schema("&4Treem25valuej8childrena^4Tree").unwrap();
        // Top is the named-schema declaration: Map with two fields.
        assert_eq!(s.serial_type, SerialType::Map);
        assert_eq!(s.size, 2);
        assert_eq!(s.keys, vec!["value", "children"]);
        assert_eq!(s.name.as_deref(), Some("Tree"));
        // The "children" field is an array whose element schema is a
        // back-reference to "Tree". The Recur node carries the target
        // name; consumers resolve it by walking the enclosing path.
        let children_arr = &s.parameters[1];
        assert_eq!(children_arr.serial_type, SerialType::Array);
        let elem = &children_arr.parameters[0];
        assert_eq!(elem.serial_type, SerialType::Recur);
        assert_eq!(elem.name.as_deref(), Some("Tree"));
    }

    #[test]
    fn test_parse_recursive_ll_optional() {
        // LL {head::Int, tail::?LL}
        //   &2LLm24headj4tail?^2LL
        let s = parse_schema("&2LLm24headj4tail?^2LL").unwrap();
        assert_eq!(s.serial_type, SerialType::Map);
        assert_eq!(s.keys, vec!["head", "tail"]);
        assert_eq!(s.name.as_deref(), Some("LL"));
        let tail_opt = &s.parameters[1];
        assert_eq!(tail_opt.serial_type, SerialType::Optional);
        let inner = &tail_opt.parameters[0];
        assert_eq!(inner.serial_type, SerialType::Recur);
        assert_eq!(inner.name.as_deref(), Some("LL"));
    }

    #[test]
    fn test_roundtrip_recursive_tree() {
        // Without materialization the wire form round-trips byte-for-byte.
        let input = "&4Treem25valuej8childrena^4Tree";
        let schema = parse_schema(input).unwrap();
        let rendered = schema_to_string(&schema);
        assert_eq!(rendered, input, "byte-exact round-trip");
    }

    #[test]
    fn test_roundtrip_recursive_ll() {
        // Optional-guarded recursion also round-trips byte-exactly.
        let input = "&2LLm24headj4tail?^2LL";
        let schema = parse_schema(input).unwrap();
        let rendered = schema_to_string(&schema);
        assert_eq!(rendered, input, "byte-exact round-trip");
    }

    #[test]
    fn test_parse_dangling_backref_rejected() {
        // A back-ref to an undeclared name must fail cleanly, not
        // panic or dereference into garbage.
        let result = parse_schema("^4Tree");
        assert!(result.is_err(), "dangling back-ref must be rejected");
    }

    // --- Wide-schema counts (>= 64) ------------------------------------
    //
    // The compiler encodes counts and key lengths with an escape for
    // values that do not fit one base-62 digit
    // (`Morloc.CodeGenerator.Serial.encode64`):
    //
    //     encode64 i | i < 64     = <one base-62 digit>
    //                | otherwise  = '=' : encode64 (i `mod` 64)
    //                                  ++ encode64 (i `div` 64)
    //
    // `hs_encode64` is an independent transcription of that function, used
    // here as an oracle so these tests describe the wire format rather
    // than whatever this module happens to implement.

    fn hs_encode64(i: usize) -> String {
        match i {
            0..=9 => ((b'0' + i as u8) as char).to_string(),
            10..=35 => ((b'a' + (i - 10) as u8) as char).to_string(),
            36..=61 => ((b'A' + (i - 36) as u8) as char).to_string(),
            62 => "+".to_string(),
            63 => "/".to_string(),
            _ => format!("={}{}", hs_encode64(i % 64), hs_encode64(i / 64)),
        }
    }

    #[test]
    fn test_hs_encode64_oracle() {
        assert_eq!(hs_encode64(0), "0");
        assert_eq!(hs_encode64(63), "/");
        assert_eq!(hs_encode64(64), "=01");
        assert_eq!(hs_encode64(255), "=/3");
        assert_eq!(hs_encode64(256), "=04");
    }

    #[test]
    fn test_parse_wide_tuple() {
        for n in [63usize, 64, 255, 256] {
            let schema_str = format!("t{}{}", hs_encode64(n), "i4".repeat(n));
            let s = parse_schema(&schema_str)
                .unwrap_or_else(|e| panic!("tuple of {n} fields failed to parse: {e:?}"));
            assert_eq!(s.serial_type, SerialType::Tuple, "tuple of {n}");
            assert_eq!(s.parameters.len(), n, "tuple of {n} arity");
        }
    }

    #[test]
    fn test_parse_wide_map() {
        for n in [63usize, 64, 255, 256] {
            let mut schema_str = format!("m{}", hs_encode64(n));
            for i in 0..n {
                let key = format!("k{i}");
                schema_str.push_str(&hs_encode64(key.len()));
                schema_str.push_str(&key);
                schema_str.push_str("i4");
            }
            let s = parse_schema(&schema_str)
                .unwrap_or_else(|e| panic!("record of {n} fields failed to parse: {e:?}"));
            assert_eq!(s.parameters.len(), n, "record of {n} arity");
            assert_eq!(s.keys.len(), n, "record of {n} keys");
            assert_eq!(s.keys[n - 1], format!("k{}", n - 1));
        }
    }

    #[test]
    fn test_parse_long_record_key() {
        // A field name of 64 characters needs the escape in its length slot.
        for klen in [63usize, 64, 100] {
            let key = "k".repeat(klen);
            let schema_str = format!("m1{}{}i4", hs_encode64(klen), key);
            let s = parse_schema(&schema_str)
                .unwrap_or_else(|e| panic!("record key of {klen} chars failed to parse: {e:?}"));
            assert_eq!(s.keys[0], key, "key of {klen} chars");
        }
    }

    #[test]
    fn test_parse_long_recursive_name() {
        // `&<klen><name>` and `^<klen><name>` share the key-length encoding.
        let name = "N".repeat(70);
        let schema_str = format!(
            "&{}{}m1{}{}?^{}{}",
            hs_encode64(name.len()),
            name,
            hs_encode64(4),
            "next",
            hs_encode64(name.len()),
            name
        );
        let s = parse_schema(&schema_str)
            .unwrap_or_else(|e| panic!("long recursive name failed to parse: {e:?}"));
        assert_eq!(s.name.as_deref(), Some(name.as_str()));
    }

    #[test]
    fn test_render_wide_schema_roundtrips() {
        // Rendering must not silently emit a filler byte for a count it
        // cannot fit in one digit; the result has to parse back.
        for n in [63usize, 64, 256] {
            let schema_str = format!("t{}{}", hs_encode64(n), "i4".repeat(n));
            let s = parse_schema(&schema_str).unwrap();
            let rendered = schema_to_string(&s);
            assert_eq!(rendered, schema_str, "render of a {n}-field tuple");
            let reparsed = parse_schema(&rendered)
                .unwrap_or_else(|e| panic!("rendered {n}-field tuple did not reparse: {e:?}"));
            assert_eq!(reparsed.parameters.len(), n);
        }
    }

    #[test]
    fn test_record_of_fixed_fields_is_fixed_width() {
        // A record's voidstar layout IS a tuple's -- `make_map_schema` and
        // `make_tuple_schema` both call `calculate_tuple_layout`, and the
        // field names live in the schema, not the buffer. So a record whose
        // fields are all fixed-width has a fixed total width and no
        // out-of-line data, exactly as the matching tuple does.
        let rec = parse_schema("m21xi41yi4").unwrap();
        let tup = parse_schema("t2i4i4").unwrap();
        assert_eq!(rec.serial_type, SerialType::Map);
        assert_eq!(rec.width, tup.width, "record and tuple widths must agree");
        assert_eq!(rec.offsets, tup.offsets, "and so must their offsets");
        assert!(
            rec.is_fixed_width(),
            "a record of fixed-width fields is fixed-width, like the tuple it is laid out as"
        );
    }

    #[test]
    fn test_record_with_variable_field_is_not_fixed_width() {
        // One variable-length field is enough to disqualify the whole
        // record, the same rule Tuple applies.
        for schema_str in ["m21xi41ys", "m21xi41yai4", "m21xi41y?i4"] {
            let s = parse_schema(schema_str).unwrap();
            assert!(
                !s.is_fixed_width(),
                "{schema_str} has a variable-width field and must not be fixed-width"
            );
        }
    }

    #[test]
    fn test_array_of_fixed_records_is_flat() {
        // The payoff: an array of fixed-field records can be bulk-copied
        // rather than walked element by element.
        let arr = parse_schema("am21xi41yi4").unwrap();
        assert!(
            arr.array_data_is_flat(),
            "[{{x::I32, y::I32}}] must take the bulk path"
        );
        let arr_var = parse_schema("am21xi41ys").unwrap();
        assert!(
            !arr_var.array_data_is_flat(),
            "a record with a Str field must still be walked"
        );
    }

    #[test]
    fn test_parse_enum() {
        // `data DNA = A | C | G | T`
        let s = parse_schema("e41A1C1G1T").unwrap();
        assert_eq!(s.serial_type, SerialType::Enum);
        assert_eq!(s.size, 4);
        assert_eq!(s.keys, vec!["A", "C", "G", "T"]);
        assert!(s.parameters.is_empty(), "an enum constructor has no payload");
    }

    #[test]
    fn test_enum_is_one_byte_and_flat() {
        // The representation claim: one byte, fixed-width, so an array of
        // them is bulk-copied exactly as a [U8] is.
        let s = parse_schema("e41A1C1G1T").unwrap();
        assert_eq!(s.width, 1, "enum width");
        assert_eq!(s.alignment(), 1, "enum alignment");
        assert!(s.is_fixed_width(), "enum must be fixed-width");

        let arr = parse_schema("ae41A1C1G1T").unwrap();
        assert!(arr.array_data_is_flat(), "[DNA] must take the flat path");
        let bytes = parse_schema("au1").unwrap();
        assert_eq!(
            arr.parameters[0].width, bytes.parameters[0].width,
            "[DNA] and [U8] must have the same element width"
        );
    }

    #[test]
    fn test_enum_roundtrips_through_render() {
        for schema_str in ["e1", "e41A1C1G1T", "e21Aa_long_constructor_name"] {
            let parsed = parse_schema(schema_str);
            if let Ok(p) = parsed {
                assert_eq!(schema_to_string(&p), schema_str, "render of {schema_str}");
            }
        }
    }

    #[test]
    fn test_enum_at_the_count_boundary() {
        // 64 constructors needs the escaped count; 256 is the cap.
        for n in [63usize, 64, 256] {
            let mut schema_str = format!("e{}", hs_encode64(n));
            for i in 0..n {
                let key = format!("C{i}");
                schema_str.push_str(&hs_encode64(key.len()));
                schema_str.push_str(&key);
            }
            let s = parse_schema(&schema_str)
                .unwrap_or_else(|e| panic!("enum of {n} constructors failed: {e:?}"));
            assert_eq!(s.keys.len(), n);
            assert_eq!(s.width, 1, "width stays one byte at {n} constructors");
            assert_eq!(schema_to_string(&s), schema_str);
        }
    }

    #[test]
    fn test_enum_over_256_is_rejected() {
        let n = 257usize;
        let mut schema_str = format!("e{}", hs_encode64(n));
        for i in 0..n {
            let key = format!("C{i}");
            schema_str.push_str(&hs_encode64(key.len()));
            schema_str.push_str(&key);
        }
        assert!(
            parse_schema(&schema_str).is_err(),
            "an enum past the one-byte tag limit must be rejected"
        );
    }

    #[test]
    fn test_malformed_count_is_rejected() {
        // A truncated escape must be an error, never a silent zero.
        assert!(parse_schema("t=").is_err(), "bare '=' count");
        assert!(parse_schema("t=0").is_err(), "escape missing high digit");
    }
}

#[cfg(test)]
mod compat_tests {
    use super::*;

    fn dump(label: &str, s: &Schema, depth: usize) {
        let indent = "  ".repeat(depth);
        print!("{}{}: type={} size={} width={}", indent, label, s.serial_type as u32, s.size, s.width);
        if !s.offsets.is_empty() {
            print!(" offsets={:?}", s.offsets);
        }
        if let Some(ref h) = s.hint { print!(" hint=\"{}\"", h); }
        if !s.keys.is_empty() { print!(" keys={:?}", s.keys); }
        println!();
        for (i, p) in s.parameters.iter().enumerate() {
            dump(&format!("param[{}]", i), p, depth + 1);
        }
    }

    #[test]
    fn test_schema_compat_with_c() {
        // Root-level metadata for representative schemas. Optional's
        // slot is a single relative pointer (width = sizeof(RelPtr)
        // = 8) regardless of inner type -- see
        // [`make_optional_schema`] for the reasoning around
        // recursive Optionals.
        let cases = vec![
            ("s", "type=13 size=1 width=16"),
            ("ai4", "type=14 size=1 width=16"),
            ("t2i4s", "type=15 size=2 width=24"),
            ("?i4", "type=17 size=1 width=8"),
            ("?s", "type=17 size=1 width=8"),
        ];
        for (input, expected_root) in &cases {
            let s = parse_schema(input).unwrap();
            let got = format!("type={} size={} width={}", s.serial_type as u32, s.size, s.width);
            assert_eq!(&got, *expected_root, "Schema '{}' mismatch", input);
        }

        // Verify tuple offsets
        let t = parse_schema("t2i4s").unwrap();
        assert_eq!(t.offsets, vec![0, 8], "t2i4s offsets");

        // Optional schemas use a single relative-pointer slot at the
        // root with no internal offset table (the inner value lives
        // elsewhere in the buffer and the relptr addresses it).
        // See [`make_optional_schema`] for details.
        let o = parse_schema("?i4").unwrap();
        assert!(o.offsets.is_empty(), "?i4 offsets");
        let os = parse_schema("?s").unwrap();
        assert!(os.offsets.is_empty(), "?s offsets");

        // Verify string has uint8 parameter
        let s = parse_schema("s").unwrap();
        assert_eq!(s.parameters.len(), 1);
        assert_eq!(s.parameters[0].serial_type, SerialType::Uint8);
        assert_eq!(s.parameters[0].width, 1);
    }

}
