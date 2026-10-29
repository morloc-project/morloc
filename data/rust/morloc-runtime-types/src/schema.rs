use crate::error::MorlocError;

/// Declares `SerialType` and `SerialType::ALL` from one list of variants,
/// so no kind can be declared without being listed.
macro_rules! serial_types {
    ($($(#[$meta:meta])* $name:ident = $tag:expr,)*) => {
        /// Morloc serial type identifiers, matching the C enum morloc_serial_type.
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        #[repr(u32)]
        pub enum SerialType {
            $($(#[$meta])* $name = $tag,)*
        }

        impl SerialType {
            /// Every kind.
            pub const ALL: &'static [SerialType] = &[$(SerialType::$name,)*];
        }
    };
}

serial_types! {
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
    IFile = 21,     // Random-access stream-file handle (read-only).
    OStream = 22,   // Sequential stream-file writer handle.
    IStream = 23,   // Sequential stream-file reader handle.
    Variant = 26,   // A `data` type with at least one constructor that takes
                    // arguments. Sixteen bytes: a tag byte, padding, and a
                    // relative pointer to the payload.
                    // Tag == declaration ordinal, as for Enum.
    Enum = 25,      // A `data` type whose constructors take no arguments. One
                    // byte: the constructor's 0-based position in the
                    // declaration, which is the wire tag.
                    //
                    // Tag 12 is retired and must not be reused.
                    //
                    // Declaration order is part of the type's wire contract:
                    // appending a constructor keeps every existing value
                    // byte-identical, reordering does not.
}

impl SerialType {
    /// The kind a C schema's `u32` tag names, or `None` for an unknown tag.
    pub fn from_u32(tag: u32) -> Option<SerialType> {
        SerialType::ALL.iter().copied().find(|k| *k as u32 == tag)
    }
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
const SCHEMA_VARIANT: u8 = b'v';

/// Recursive schema definition, mirroring the C Schema struct.
#[derive(Debug, Clone)]
pub struct Schema {
    pub serial_type: SerialType,
    /// Number of parameters, or of constructors for an enum.
    pub size: usize,
    /// Inline byte width of a value of this type.
    pub width: usize,
    /// Field offsets of a tuple or record; an array's fixed length, if any.
    pub offsets: Vec<usize>,
    /// Optional type hint string.
    pub hint: Option<String>,
    /// Child schemas (element type for arrays, field types for tuples, etc.).
    pub parameters: Vec<Schema>,
    /// Record field, table column or constructor names.
    pub keys: Vec<String>,
    /// The declared name on the outer node of a `&<klen><name>` declaration,
    /// or the referenced name on a `Recur` node; otherwise `None`.
    pub name: Option<String>,
}

impl Schema {
    /// A table schema declaring the given columns (none for the bare
    /// `T` form).
    pub fn table(params: Vec<Schema>, keys: Vec<String>) -> Self {
        make_table_schema(params, keys)
    }

    /// The fixed length of an array schema (`a:<n>`), if it has one.
    pub fn array_dim(&self) -> Option<usize> {
        if self.serial_type == SerialType::Array {
            self.offsets.first().copied()
        } else {
            None
        }
    }

    /// A list of `elem` with no fixed length (FMT-6).
    pub fn array(elem: Schema) -> Self {
        make_array_schema_with_dim(None, elem)
    }

    /// A tuple of `params` (FMT-6).
    pub fn tuple(params: Vec<Schema>) -> Self {
        make_tuple_schema(params)
    }

    /// A record of `params` named by `keys` (FMT-6).
    pub fn record(params: Vec<Schema>, keys: Vec<String>) -> Self {
        make_map_schema(params, keys)
    }

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
            SerialType::Array | SerialType::Tuple | SerialType::Map | SerialType::Optional | SerialType::Table | SerialType::Recur | SerialType::Variant => 0,
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
            // Records share tuple layout. Treating them as fixed-width means the
            // padding between fields is copied and hashed with the value, which
            // relies on SHM-6 (a new block reads as zero).
            SerialType::Tuple | SerialType::Map => {
                self.parameters.iter().all(|p| p.is_fixed_width())
            }
            SerialType::Enum => true,
            SerialType::Optional => false,
            SerialType::Recur => false,
            SerialType::String | SerialType::Array | SerialType::Int | SerialType::Table | SerialType::IFile | SerialType::OStream | SerialType::IStream | SerialType::Variant => false,
        }
    }

    /// True when this String/Array schema's data is one contiguous block of
    /// `size * elem_width` bytes that can be copied or hashed in bulk.
    /// Not `is_fixed_width()`, which is false for every Array.
    pub fn array_data_is_flat(&self) -> bool {
        match self.serial_type {
            SerialType::String => true,
            SerialType::Array => self.parameters.first().map_or(false, |e| e.is_fixed_width()),
            SerialType::Nil | SerialType::Bool | SerialType::Sint8 | SerialType::Sint16 | SerialType::Sint32 | SerialType::Sint64 | SerialType::Uint8 | SerialType::Uint16 | SerialType::Uint32 | SerialType::Uint64 | SerialType::Float32 | SerialType::Float64 | SerialType::Tuple | SerialType::Map | SerialType::Optional | SerialType::Int | SerialType::Table | SerialType::Recur | SerialType::IFile | SerialType::OStream | SerialType::IStream | SerialType::Variant | SerialType::Enum => false,
        }
    }

    /// Alignment requirement for this type.
    pub fn alignment(&self) -> usize {
        match self.serial_type {
            SerialType::Nil => 1,
            SerialType::Bool | SerialType::Sint8 | SerialType::Uint8 | SerialType::Enum => 1,
            SerialType::Sint16 | SerialType::Uint16 => 2,
            SerialType::Sint32 | SerialType::Uint32 | SerialType::Float32 => 4,
            SerialType::Sint64 | SerialType::Uint64 | SerialType::Float64 => 8,
            SerialType::String | SerialType::Array | SerialType::Map
            | SerialType::Int | SerialType::Table | SerialType::IFile
            | SerialType::OStream | SerialType::IStream => {
                std::mem::size_of::<usize>() // pointer-sized alignment
            }
            SerialType::Recur => std::mem::size_of::<usize>(),
            SerialType::Tuple => {
                self.parameters
                    .iter()
                    .map(|p| p.alignment())
                    .max()
                    .unwrap_or(1)
            }
            SerialType::Optional | SerialType::Variant => {
                std::mem::size_of::<usize>()
            }
        }
    }

    /// True if this schema is a primitive numeric type.
    pub fn is_primitive_numeric(&self) -> bool {
        matches!(
            self.serial_type,
            SerialType::Sint8 | SerialType::Sint16 | SerialType::Sint32 | SerialType::Sint64
                | SerialType::Uint8 | SerialType::Uint16 | SerialType::Uint32 | SerialType::Uint64
                | SerialType::Float32 | SerialType::Float64
        )
    }

    /// Alignment of an Array's element data buffer: 64 bytes for primitive
    /// numerics, otherwise the natural alignment.
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
/// Grammar (positional, no separators):
/// - `z` Nil, `b` Bool, `s` String, `j` Int (arbitrary width)
/// - `i<n>`, `u<n>` signed, unsigned integer of n bytes (1, 2, 4, 8); `f4`, `f8`
/// - `a<s>` array of `s`; `a:<n><s>` with fixed length n (decimal)
/// - `?<s>` optional
/// - `t<c><s>...` tuple of c fields
/// - `m<c>(<k><key><s>)...` record of c named fields
/// - `e<c>(<k><name>)...` enum of c constructors
/// - `v<c>(<k><name><a><s>...)...` variant of c arms, each of arity a
/// - `T` table with no declared columns; `T:<c>(<k><key><s>)...` with c
/// - `F`, `O`, `I` IFile, OStream, IStream handles
/// - `&<k><name><s>` declares `name` for `s`; `^<k><name>` refers back to it
/// - `<hint><s>` attaches a language type hint
///
/// Counts c, a and key lengths k are one digit from `0-9a-zA-Z+/` (0-63), or
/// an escape for 64 or more (see `read_count`). Key lengths count UTF-8 bytes.
/// Example: `m24names1xi4` is a record {name: String, x: Sint32}.
pub fn parse_schema(input: &str) -> Result<Schema, MorlocError> {
    let bytes = input.as_bytes();
    let mut declared: Vec<String> = Vec::new();
    let (schema, consumed) = parse_schema_r(bytes, 0, &mut declared, 0)?;
    if consumed != bytes.len() {
        return Err(MorlocError::Schema(format!(
            "trailing characters after schema at position {consumed}"
        )));
    }
    Ok(schema)
}

/// Deepest nesting a schema string may have. Every walk over a schema
/// recurses once per level, so the depth is bounded on the way in.
pub const MAX_SCHEMA_DEPTH: usize = 128;

/// `declared` holds the names of the enclosing `&` declarations, so a
/// back-reference can name only an enclosing declaration.
///
/// The container arms are `#[inline(never)]` so that one nesting level
/// costs this frame plus one arm's frame, not every arm's temporaries.
fn parse_schema_r(
    bytes: &[u8],
    pos: usize,
    declared: &mut Vec<String>,
    depth: usize,
) -> Result<(Schema, usize), MorlocError> {
    if pos >= bytes.len() {
        return Err(MorlocError::Schema("unexpected end of schema".into()));
    }
    if depth > MAX_SCHEMA_DEPTH {
        return Err(MorlocError::Schema(format!(
            "schema nesting exceeds {MAX_SCHEMA_DEPTH} levels"
        )));
    }

    let c = bytes[pos];
    let cur = pos + 1;

    match c {
        b'<' => parse_hint_arm(bytes, cur, declared, depth),
        b'&' => parse_declaration_arm(bytes, cur, declared, depth),
        b'^' => parse_back_reference_arm(bytes, cur, declared),
        SCHEMA_NIL => Ok((Schema::primitive(SerialType::Nil), cur)),
        SCHEMA_BOOL => Ok((Schema::primitive(SerialType::Bool), cur)),
        SCHEMA_STRING => Ok((make_string_schema(), cur)),
        SCHEMA_IFILE => parse_stream_handle_arm(bytes, cur, SerialType::IFile, depth),
        SCHEMA_OSTREAM => parse_stream_handle_arm(bytes, cur, SerialType::OStream, depth),
        SCHEMA_ISTREAM => parse_stream_handle_arm(bytes, cur, SerialType::IStream, depth),
        SCHEMA_SINT => parse_sized_int(bytes, cur, true),
        SCHEMA_UINT => parse_sized_int(bytes, cur, false),
        SCHEMA_FLOAT => parse_sized_float(bytes, cur),
        SCHEMA_ARRAY => parse_array_arm(bytes, cur, declared, depth),
        SCHEMA_INT => Ok((Schema::primitive(SerialType::Int), cur)),
        SCHEMA_OPTIONAL => parse_optional_arm(bytes, cur, declared, depth),
        SCHEMA_TUPLE => parse_tuple_arm(bytes, cur, declared, depth),
        SCHEMA_MAP => parse_map_arm(bytes, cur, declared, depth),
        SCHEMA_ENUM => parse_enum_arm(bytes, cur),
        SCHEMA_VARIANT => parse_variant_arm(bytes, cur, declared, depth),
        SCHEMA_TABLE => parse_table_arm(bytes, cur, declared, depth),
        _ => Err(MorlocError::Schema(format!(
            "unknown schema character '{}' at position {pos}",
            c as char
        ))),
    }
}

fn make_string_schema() -> Schema {
    Schema {
        serial_type: SerialType::String,
        size: 1,
        width: std::mem::size_of::<crate::shm_types::Array>(),
        offsets: Vec::new(),
        hint: None,
        parameters: vec![Schema::primitive(SerialType::Uint8)],
        keys: Vec::new(),
        name: None,
    }
}

/// A handle's one key is the canonical schema of what its file stores.
fn make_stream_handle_schema(st: SerialType, storage: String) -> Schema {
    Schema {
        serial_type: st,
        size: 1,
        width: std::mem::size_of::<crate::shm_types::Array>(),
        offsets: Vec::new(),
        hint: None,
        parameters: vec![Schema::primitive(SerialType::Uint8)],
        keys: vec![storage],
        name: None,
    }
}

/// `F`, `O` or `I`, then the counted schema the handle's file stores. That
/// schema is closed: it is parsed on its own, with no enclosing declarations.
#[inline(never)]
fn parse_stream_handle_arm(
    bytes: &[u8],
    cur: usize,
    st: SerialType,
    depth: usize,
) -> Result<(Schema, usize), MorlocError> {
    let (storage, end) = parse_named_key(bytes, cur).map_err(|e| {
        MorlocError::Schema(format!("a stream handle schema must name what its file stores: {e}"))
    })?;
    let mut declared: Vec<String> = Vec::new();
    let (parsed, used) = parse_schema_r(storage.as_bytes(), 0, &mut declared, depth + 1)?;
    if used != storage.len() {
        return Err(MorlocError::Schema(format!(
            "trailing characters in stream handle storage schema '{storage}'"
        )));
    }
    Ok((make_stream_handle_schema(st, schema_to_string(&parsed)), end))
}

/// The canonical schema a handle's file stores, or None for a schema that
/// is not a handle.
pub fn handle_storage(schema: &Schema) -> Option<&str> {
    let is_handle = schema.serial_type == SerialType::IFile
        || schema.serial_type == SerialType::OStream
        || schema.serial_type == SerialType::IStream;
    if is_handle { schema.keys.first().map(String::as_str) } else { None }
}

/// Hint: `<...>` with nesting support, then the actual type.
#[inline(never)]
fn parse_hint_arm(
    bytes: &[u8],
    cur: usize,
    declared: &mut Vec<String>,
    depth: usize,
) -> Result<(Schema, usize), MorlocError> {
    let (hint, after_hint) = parse_hint(bytes, cur)?;
    let (mut schema, end) = parse_schema_r(bytes, after_hint, declared, depth + 1)?;
    schema.hint = Some(hint);
    Ok((schema, end))
}

/// Named-schema declaration: `&<klen><name>X`. Every `^<klen><name>`
/// back-reference in the body gets the declaration's width, since an
/// array of `Recur(T)` strides by T's width.
#[inline(never)]
fn parse_declaration_arm(
    bytes: &[u8],
    cur: usize,
    declared: &mut Vec<String>,
    depth: usize,
) -> Result<(Schema, usize), MorlocError> {
    let (name, after_name) = parse_named_key(bytes, cur)?;
    declared.push(name.clone());
    let parsed = parse_schema_r(bytes, after_name, declared, depth + 1);
    declared.pop();
    let (mut body, end) = parsed?;
    // A back-reference reachable through tuple/record fields alone would put
    // the value inside itself. Arrays, optionals and variant payloads sit
    // behind a pointer.
    if reaches_inline_recur(&body, &name) {
        return Err(MorlocError::Schema(format!(
            "declaration '{name}' contains itself inline (a back-reference \
             reachable through tuple or record fields only)"
        )));
    }
    // Enclosing Tuple/Map widths depend on the patched Recur widths; repeat
    // until the declaration's width settles.
    for _ in 0..16 {
        let prev = body.width;
        patch_recur_widths_in(&mut body, &name, prev);
        recalculate_container_widths(&mut body);
        if body.width == prev { break; }
    }
    body.name = Some(name);
    Ok((body, end))
}

/// Back-reference: `^<klen><name>`, naming an enclosing declaration.
#[inline(never)]
fn parse_back_reference_arm(
    bytes: &[u8],
    cur: usize,
    declared: &mut Vec<String>,
) -> Result<(Schema, usize), MorlocError> {
    let (name, after_name) = parse_named_key(bytes, cur)?;
    if !declared.iter().any(|n| *n == name) {
        return Err(MorlocError::Schema(format!(
            "back-reference to undeclared name '{name}'"
        )));
    }
    Ok((make_recur_schema(name), after_name))
}

/// Array: optional dimension constraint (`:N` in decimal), then the child
/// schema.
#[inline(never)]
fn parse_array_arm(
    bytes: &[u8],
    mut cur: usize,
    declared: &mut Vec<String>,
    depth: usize,
) -> Result<(Schema, usize), MorlocError> {
    let expected_len = if cur < bytes.len() && bytes[cur] == b':' {
        cur += 1;
        let (n, after) = parse_decimal(bytes, cur)?;
        cur = after;
        Some(n)
    } else {
        None
    };
    let (child, end) = parse_schema_r(bytes, cur, declared, depth + 1)?;
    Ok((make_array_schema_with_dim(expected_len, child), end))
}

/// Optional: one child schema follows immediately.
#[inline(never)]
fn parse_optional_arm(
    bytes: &[u8],
    cur: usize,
    declared: &mut Vec<String>,
    depth: usize,
) -> Result<(Schema, usize), MorlocError> {
    let (child, end) = parse_schema_r(bytes, cur, declared, depth + 1)?;
    Ok((make_optional_schema(child), end))
}

/// Tuple: count, then N child schemas.
#[inline(never)]
fn parse_tuple_arm(
    bytes: &[u8],
    cur: usize,
    declared: &mut Vec<String>,
    depth: usize,
) -> Result<(Schema, usize), MorlocError> {
    if cur >= bytes.len() {
        return Err(MorlocError::Schema("expected tuple size".into()));
    }
    let (n, mut p) = read_count(bytes, cur)?;
    let mut params = Vec::with_capacity(n);
    for _ in 0..n {
        let (child, end) = parse_schema_r(bytes, p, declared, depth + 1)?;
        params.push(child);
        p = end;
    }
    Ok((make_tuple_schema(params), p))
}

/// Read a count-prefixed key at `p`, returning it and the position just
/// past it. `what` names the key kind in error messages.
fn read_prefixed_key(bytes: &[u8], p: usize, what: &str) -> Result<(String, usize), MorlocError> {
    if p >= bytes.len() {
        return Err(MorlocError::Schema(format!("expected {what} length")));
    }
    let (klen, kp) = read_count(bytes, p)?;
    if kp + klen > bytes.len() {
        return Err(MorlocError::Schema(format!("{what} extends past end")));
    }
    let key = std::str::from_utf8(&bytes[kp..kp + klen])
        .map_err(|_| MorlocError::Schema(format!("invalid UTF-8 in {what}")))?
        .to_string();
    Ok((key, kp + klen))
}

/// Map/record: count, then N (key length, key bytes, value schema).
#[inline(never)]
fn parse_map_arm(
    bytes: &[u8],
    cur: usize,
    declared: &mut Vec<String>,
    depth: usize,
) -> Result<(Schema, usize), MorlocError> {
    if cur >= bytes.len() {
        return Err(MorlocError::Schema("expected map size".into()));
    }
    let (n, mut p) = read_count(bytes, cur)?;
    let mut params = Vec::with_capacity(n);
    let mut keys = Vec::with_capacity(n);
    for _ in 0..n {
        let (key, kp) = read_prefixed_key(bytes, p, "map key")?;
        keys.push(key);
        let (child, end) = parse_schema_r(bytes, kp, declared, depth + 1)?;
        params.push(child);
        p = end;
    }
    Ok((make_map_schema(params, keys), p))
}

/// Enum: count, then N (key_len + constructor name). No child schemas: an
/// argument-free constructor carries no payload.
#[inline(never)]
fn parse_enum_arm(bytes: &[u8], cur: usize) -> Result<(Schema, usize), MorlocError> {
    let (n, mut p) = read_count(bytes, cur)?;
    let mut keys = Vec::with_capacity(n);
    for _ in 0..n {
        let (key, kp) = read_prefixed_key(bytes, p, "enum constructor name")?;
        keys.push(key);
        p = kp;
    }
    if keys.len() > 256 {
        return Err(MorlocError::Schema(format!(
            "enum has {} constructors; the limit is 256 so a tag fits in one byte",
            keys.len()
        )));
    }
    Ok((make_enum_schema(keys), p))
}

/// Variant: count, then N (key_len + name + arity + arity schemas). Each
/// arm becomes a Tuple of its field schemas, so an argument-free arm is the
/// empty tuple.
#[inline(never)]
fn parse_variant_arm(
    bytes: &[u8],
    cur: usize,
    declared: &mut Vec<String>,
    depth: usize,
) -> Result<(Schema, usize), MorlocError> {
    let (n, mut p) = read_count(bytes, cur)?;
    let mut keys = Vec::with_capacity(n);
    let mut params = Vec::with_capacity(n);
    for _ in 0..n {
        let (key, kp) = read_prefixed_key(bytes, p, "variant arm name")?;
        let (arity, ap) = read_count(bytes, kp)?;
        p = ap;
        let mut fields = Vec::with_capacity(arity);
        for _ in 0..arity {
            let (child, end) = parse_schema_r(bytes, p, declared, depth + 1)?;
            fields.push(child);
            p = end;
        }
        keys.push(key);
        params.push(make_tuple_schema(fields));
    }
    if keys.len() > 256 {
        return Err(MorlocError::Schema(format!(
            "variant has {} arms; the limit is 256 so a tag fits in one byte",
            keys.len()
        )));
    }
    Ok((make_variant_schema(params, keys), p))
}

/// Table (Arrow IPC): bare `T` declares no columns and accepts any buffer;
/// `T:K<entries>` declares K columns, with entries as in `m`. Declared
/// columns are minimum constraints; the buffer may carry more.
#[inline(never)]
fn parse_table_arm(
    bytes: &[u8],
    mut cur: usize,
    declared: &mut Vec<String>,
    depth: usize,
) -> Result<(Schema, usize), MorlocError> {
    if cur < bytes.len() && bytes[cur] == b':' {
        cur += 1;
        if cur >= bytes.len() {
            return Err(MorlocError::Schema("expected table column count after ':'".into()));
        }
        let (n, mut p) = read_count(bytes, cur)?;
        let mut params = Vec::with_capacity(n);
        let mut keys = Vec::with_capacity(n);
        for _ in 0..n {
            let (key, kp) = read_prefixed_key(bytes, p, "table column key")?;
            keys.push(key);
            let (child, end) = parse_schema_r(bytes, kp, declared, depth + 1)?;
            params.push(child);
            p = end;
        }
        Ok((make_table_schema(params, keys), p))
    } else {
        Ok((make_table_schema(Vec::new(), Vec::new()), cur))
    }
}

/// Parse a `<klen><name>` length-prefixed identifier, the same syntax used
/// for record keys but in the slots after `&` and `^` markers. Returns the
/// name string and the position just past its last byte.
fn parse_named_key(bytes: &[u8], pos: usize) -> Result<(String, usize), MorlocError> {
    if pos >= bytes.len() {
        return Err(MorlocError::Schema("expected name length".into()));
    }
    let (klen, start) = read_count(bytes, pos)?;
    let end = match start.checked_add(klen) {
        Some(end) if end <= bytes.len() => end,
        _ => return Err(MorlocError::Schema("schema name extends past end".into())),
    };
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

/// An array's fixed length, if it has one, is its only offset.
fn make_array_schema_with_dim(expected_len: Option<usize>, child: Schema) -> Schema {
    Schema {
        serial_type: SerialType::Array,
        size: 1,
        width: std::mem::size_of::<crate::shm_types::Array>(),
        offsets: expected_len.into_iter().collect(),
        hint: None,
        parameters: vec![child],
        keys: Vec::new(),
        name: None,
    }
}

fn make_optional_schema(child: Schema) -> Schema {
    // One relative pointer, RELNULL when absent, so the slot width does not
    // depend on the child's and recursive widths terminate.
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

/// Construct an Enum schema from its constructor names: one byte,
/// fixed-width.
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

/// Construct a Variant schema: a tag byte, padding and a relptr to the
/// payload, whatever the arms hold.
fn make_variant_schema(params: Vec<Schema>, keys: Vec<String>) -> Schema {
    Schema {
        serial_type: SerialType::Variant,
        size: params.len(),
        width: 16,
        offsets: Vec::new(),
        hint: None,
        parameters: params,
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

/// A `Recur` node naming `name`. Its width is a placeholder until
/// `parse_declaration_arm` patches it to the declaration's width.
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

/// True when a `Recur(name)` node is reachable from `schema` through
/// Tuple/Map fields alone. A nested declaration of the same name shadows
/// the outer one, so the walk stops there; its own body was checked when
/// it was parsed.
fn reaches_inline_recur(schema: &Schema, name: &str) -> bool {
    match schema.serial_type {
        SerialType::Recur => schema.name.as_deref() == Some(name),
        SerialType::Tuple | SerialType::Map => schema.parameters.iter().any(|p| match p.serial_type {
            SerialType::Recur => p.name.as_deref() == Some(name),
            _ if p.name.as_deref() == Some(name) => false,
            SerialType::Nil | SerialType::Bool | SerialType::Sint8 | SerialType::Sint16 | SerialType::Sint32 | SerialType::Sint64 | SerialType::Uint8 | SerialType::Uint16 | SerialType::Uint32 | SerialType::Uint64 | SerialType::Float32 | SerialType::Float64 | SerialType::String | SerialType::Array | SerialType::Tuple | SerialType::Map | SerialType::Optional | SerialType::Int | SerialType::Table | SerialType::IFile | SerialType::OStream | SerialType::IStream | SerialType::Variant | SerialType::Enum => reaches_inline_recur(p, name),
        }),
        SerialType::Nil | SerialType::Bool | SerialType::Sint8 | SerialType::Sint16 | SerialType::Sint32 | SerialType::Sint64 | SerialType::Uint8 | SerialType::Uint16 | SerialType::Uint32 | SerialType::Uint64 | SerialType::Float32 | SerialType::Float64 | SerialType::String | SerialType::Array | SerialType::Optional | SerialType::Int | SerialType::Table | SerialType::IFile | SerialType::OStream | SerialType::IStream | SerialType::Variant | SerialType::Enum => false,
    }
}

/// Set the width of every `Recur(name)` under `schema`, not descending
/// into a nested declaration of the same name, which owns its own
/// back-references.
fn patch_recur_widths_in(schema: &mut Schema, name: &str, width: usize) {
    for p in &mut schema.parameters {
        match p.serial_type {
            SerialType::Recur => {
                if p.name.as_deref() == Some(name) {
                    p.width = width;
                }
            }
            _ if p.name.as_deref() == Some(name) => {}
            SerialType::Nil | SerialType::Bool | SerialType::Sint8 | SerialType::Sint16 | SerialType::Sint32 | SerialType::Sint64 | SerialType::Uint8 | SerialType::Uint16 | SerialType::Uint32 | SerialType::Uint64 | SerialType::Float32 | SerialType::Float64 | SerialType::String | SerialType::Array | SerialType::Tuple | SerialType::Map | SerialType::Optional | SerialType::Int | SerialType::Table | SerialType::IFile | SerialType::OStream | SerialType::IStream | SerialType::Variant | SerialType::Enum => patch_recur_widths_in(p, name, width),
        }
    }
}

/// Recompute Tuple and Map widths and offsets bottom-up.
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
        offset = (offset + align - 1) & !(align - 1);
        offsets.push(offset);
        offset += param.width;
    }

    let width = (offset + max_align - 1) & !(max_align - 1);
    (width, offsets)
}

/// Render a schema back to its string representation.
pub fn schema_to_string(schema: &Schema) -> String {
    let mut buf = String::new();
    schema_to_string_inner(schema, &mut buf);
    buf
}

/// Structural compatibility: equality, except that an array with no fixed
/// length matches one of any length. Hints are ignored.
pub fn schemas_compatible(a: &Schema, b: &Schema) -> bool {
    if a.serial_type != b.serial_type
        || a.size != b.size
        || a.name != b.name
        || a.parameters.len() != b.parameters.len()
    {
        return false;
    }
    if let (Some(sa), Some(sb)) = (handle_storage(a), handle_storage(b)) {
        return schema_strings_compatible(sa, sb);
    }
    if a.keys != b.keys {
        return false;
    }
    if a.serial_type == SerialType::Array {
        if let (Some(la), Some(lb)) = (a.array_dim(), b.array_dim()) {
            if la != lb {
                return false;
            }
        }
    } else if a.offsets != b.offsets {
        return false;
    }
    a.parameters.iter().zip(b.parameters.iter())
        .all(|(pa, pb)| schemas_compatible(pa, pb))
}

/// Canonical form of a schema string: parsed and re-rendered, which drops
/// `<hint>` prefixes. An unparseable string is returned unchanged.
pub fn canonicalize_schema_str(s: &str) -> String {
    match parse_schema(s) {
        Ok(parsed) => schema_to_string(&parsed),
        Err(_) => s.to_string(),
    }
}

/// `schemas_compatible` on two schema strings.
pub fn schema_strings_compatible(stored: &str, requested: &str) -> bool {
    if stored == requested {
        return true;
    }
    match (parse_schema(stored), parse_schema(requested)) {
        (Ok(a), Ok(b)) => schemas_compatible(&a, &b),
        // Unparseable schemas are compatible only when identical.
        _ => stored == requested,
    }
}

fn schema_to_string_inner(schema: &Schema, buf: &mut String) {
    // A non-Recur node with a name is a declaration site.
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
        SerialType::IFile | SerialType::OStream | SerialType::IStream => {
            let code = if schema.serial_type == SerialType::IFile {
                'F'
            } else if schema.serial_type == SerialType::OStream {
                'O'
            } else {
                'I'
            };
            buf.push(code);
            let storage = schema.keys.first().map(String::as_str).unwrap_or("");
            write_count(buf, storage.len());
            buf.push_str(storage);
        }
        SerialType::Array => {
            buf.push('a');
            if let Some(expected) = schema.array_dim() {
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
        SerialType::Variant => {
            buf.push('v');
            write_count(buf, schema.size);
            for (i, arm) in schema.parameters.iter().enumerate() {
                if i < schema.keys.len() {
                    let key = &schema.keys[i];
                    write_count(buf, key.len());
                    buf.push_str(key);
                }
                // The arm is a tuple of its fields; the fields are written
                // out directly, with the tuple's size standing as the arity.
                write_count(buf, arm.size);
                for field in &arm.parameters {
                    schema_to_string_inner(field, buf);
                }
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
            buf.push('^');
            if let Some(ref n) = schema.name {
                write_count(buf, n.len());
                buf.push_str(n);
            }
        }
    }
}

/// Make a sub-schema self-contained by replacing every back-reference to
/// `parent`'s declaration with `parent` itself. A field schema handed to a
/// reader on its own has no enclosing declaration to resolve against; with
/// the parent spliced in, the parent's own back-references then sit under
/// the declaration they need.
pub fn reroot_under(parent: &Schema, child: &Schema) -> Schema {
    match (parent.serial_type, parent.name.as_deref()) {
        (SerialType::Recur, _) | (_, None) => child.clone(),
        (_, Some(name)) => splice(name, parent, child),
    }
}

fn splice(name: &str, parent: &Schema, s: &Schema) -> Schema {
    if s.serial_type == SerialType::Recur && s.name.as_deref() == Some(name) {
        return parent.clone();
    }
    let mut out = s.clone();
    out.parameters = s.parameters.iter().map(|c| splice(name, parent, c)).collect();
    out
}

/// True when a back-reference to `name` occurs anywhere in `s`.
pub fn refers_to(s: &Schema, name: &str) -> bool {
    (s.serial_type == SerialType::Recur && s.name.as_deref() == Some(name))
        || s.parameters.iter().any(|c| refers_to(c, name))
}

#[cfg(test)]
mod tests {
    #[test]
    fn from_u32_names_every_kind_and_nothing_else() {
        for &k in SerialType::ALL {
            assert_eq!(SerialType::from_u32(k as u32), Some(k));
        }
        let tags: std::collections::HashSet<u32> = SerialType::ALL.iter().map(|k| *k as u32).collect();
        assert_eq!(tags.len(), SerialType::ALL.len(), "a kind is listed twice");
        for t in 0..64u32 {
            if !tags.contains(&t) {
                assert_eq!(SerialType::from_u32(t), None, "tag {t}");
            }
        }
    }

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

    #[test]
    fn canonicalize_drops_hints_and_is_idempotent() {
        assert_eq!(canonicalize_schema_str("a<dict>m24kinds2idj"), "am24kinds2idj");
        assert_eq!(canonicalize_schema_str("a<str>s"), "as");
        assert_eq!(canonicalize_schema_str("<std::vector<$1>>ai4"), "ai4");
        assert_eq!(canonicalize_schema_str("am24kinds2idj"), "am24kinds2idj");
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
        assert!(!schema_strings_compatible("am24kinds2idj", "a<dict>m25alphas4betaj"));
        assert!(!schema_strings_compatible("aj", "as"));
        assert!(!schema_strings_compatible("aj", "j"));
        assert!(!schema_strings_compatible("t2js", "t2sj"));
        assert!(!schema_strings_compatible("t2js", "t3jss"));
    }

    // An array with no fixed length matches any fixed length.
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

    // Recursive schemas: `&<klen><name>` declares, `^<klen><name>` refers
    // back. The parser does not expand the cycle, so the form round-trips
    // byte-for-byte.

    #[test]
    fn test_parse_recursive_tree_list() {
        let s = parse_schema("&4Treem25valuej8childrena^4Tree").unwrap();
        // Top is the named-schema declaration: Map with two fields.
        assert_eq!(s.serial_type, SerialType::Map);
        assert_eq!(s.size, 2);
        assert_eq!(s.keys, vec!["value", "children"]);
        assert_eq!(s.name.as_deref(), Some("Tree"));
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
        let input = "&4Treem25valuej8childrena^4Tree";
        let schema = parse_schema(input).unwrap();
        let rendered = schema_to_string(&schema);
        assert_eq!(rendered, input, "byte-exact round-trip");
    }

    //   data Tree = Leaf | Node Tree Tree
    //     -> &4Treev24Leaf04Node2^4Tree^4Tree
    #[test]
    fn test_parse_recursive_variant() {
        let s = parse_schema("&4Treev24Leaf04Node2^4Tree^4Tree").unwrap();
        assert_eq!(s.serial_type, SerialType::Variant);
        assert_eq!(s.size, 2);
        assert_eq!(s.keys, vec!["Leaf", "Node"]);
        assert_eq!(s.name.as_deref(), Some("Tree"));
        // Tag plus pointer, independent of what the arms hold.
        assert_eq!(s.width, 16);
        assert_eq!(s.alignment(), 8);
        assert!(!s.is_fixed_width());

        let leaf = &s.parameters[0];
        assert_eq!(leaf.serial_type, SerialType::Tuple);
        assert_eq!(leaf.parameters.len(), 0);

        let node = &s.parameters[1];
        assert_eq!(node.serial_type, SerialType::Tuple);
        assert_eq!(node.parameters.len(), 2);
        for field in &node.parameters {
            assert_eq!(field.serial_type, SerialType::Recur);
            assert_eq!(field.name.as_deref(), Some("Tree"));
        }
    }

    #[test]
    fn test_recursive_variant_recur_width_patched() {
        // Back-references take the declaration's width.
        let s = parse_schema("&4Treev24Leaf04Node2^4Tree^4Tree").unwrap();
        let node = &s.parameters[1];
        for field in &node.parameters {
            assert_eq!(field.width, s.width, "back-reference width follows the declaration");
        }
        assert_eq!(node.width, 2 * s.width, "payload tuple holds two full Trees");
    }

    #[test]
    fn test_roundtrip_recursive_variant() {
        let input = "&4Treev24Leaf04Node2^4Tree^4Tree";
        let schema = parse_schema(input).unwrap();
        assert_eq!(schema_to_string(&schema), input, "byte-exact round-trip");
    }

    #[test]
    fn test_recursive_variant_without_declaration_is_rejected() {
        let err = parse_schema("v24Leaf04Node2^4Tree^4Tree");
        assert!(err.is_err(), "dangling back-reference must be refused");
    }

    #[test]
    fn test_roundtrip_recursive_ll() {
        let input = "&2LLm24headj4tail?^2LL";
        let schema = parse_schema(input).unwrap();
        let rendered = schema_to_string(&schema);
        assert_eq!(rendered, input, "byte-exact round-trip");
    }

    #[test]
    fn test_parse_dangling_backref_rejected() {
        let result = parse_schema("^4Tree");
        assert!(result.is_err(), "dangling back-ref must be rejected");
    }

    // --- Wide-schema counts (>= 64) ------------------------------------
    //
    // `hs_encode64` transcribes `Morloc.CodeGenerator.Serial.encode64` as
    // an oracle, so these tests describe the compiler's encoding.

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
    fn test_parse_variant() {
        // `data Shape = Circle Real | Rect Real Real | Dot`
        // v <count> ( <klen><name> <arity> <schema>*arity )*
        let s = parse_schema("v36Circle1f84Rect2f8f83Dot0").unwrap();
        assert_eq!(s.serial_type, SerialType::Variant);
        assert_eq!(s.size, 3);
        assert_eq!(s.keys, vec!["Circle", "Rect", "Dot"]);
        assert_eq!(s.parameters.len(), 3);
        assert_eq!(s.parameters[0].serial_type, SerialType::Tuple);
        assert_eq!(s.parameters[0].size, 1);
        assert_eq!(s.parameters[1].size, 2);
        assert_eq!(s.parameters[2].size, 0);
    }

    #[test]
    fn test_variant_slot_is_tag_plus_pointer() {
        let s = parse_schema("v36Circle1f84Rect2f8f83Dot0").unwrap();
        assert_eq!(s.width, 16, "tag byte plus padding plus a relptr");
        assert_eq!(s.alignment(), 8, "pointer-aligned");
        assert!(!s.is_fixed_width(), "the pointee is not fixed-width");

        // An arm whose fields are wider must not change the slot.
        let wide = parse_schema("v14Wide4f8f8f8f8").unwrap();
        assert_eq!(wide.width, s.width, "slot width is independent of the arms");
    }

    #[test]
    fn test_single_field_arm_is_byte_identical_to_its_field() {
        // A tuple has no header, so a 1-tuple is laid out as its field.
        let one = parse_schema("t1f8").unwrap();
        let bare = parse_schema("f8").unwrap();
        assert_eq!(one.width, bare.width);
        assert_eq!(one.offsets, vec![0]);
    }

    #[test]
    fn test_variant_roundtrips_through_render() {
        for schema_str in ["v14Only0", "v36Circle1f84Rect2f8f83Dot0", "v24Leaf04Node2i4i4"] {
            let parsed = parse_schema(schema_str)
                .unwrap_or_else(|e| panic!("{schema_str} failed to parse: {e:?}"));
            assert_eq!(schema_to_string(&parsed), schema_str);
        }
    }

    #[test]
    fn test_variant_over_256_arms_is_rejected() {
        let n = 257usize;
        let mut schema_str = format!("v{}", hs_encode64(n));
        for i in 0..n {
            let key = format!("C{i}");
            schema_str.push_str(&hs_encode64(key.len()));
            schema_str.push_str(&key);
            schema_str.push_str(&hs_encode64(0));
        }
        assert!(
            parse_schema(&schema_str).is_err(),
            "a variant past the one-byte tag limit must be rejected"
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

    #[test]
    fn test_inline_self_reference_is_rejected() {
        // A record holding itself with no pointer in between has no finite layout.
        let err = parse_schema("&1Am11a^1A").unwrap_err().to_string();
        assert!(err.contains("inline"), "{err}");
        // The same through a tuple field.
        let err = parse_schema("&1Am11at2j^1A").unwrap_err().to_string();
        assert!(err.contains("inline"), "{err}");
    }

    #[test]
    fn test_guarded_self_references_parse() {
        // Array, Optional and Variant payloads sit behind a pointer.
        assert!(parse_schema("&1Am11aa^1A").is_ok());
        assert!(parse_schema("&1Am11a?^1A").is_ok());
        assert!(parse_schema("&1Av21N01C1^1A").is_ok());
        // A back-reference inside an array of tuples is behind the array.
        assert!(parse_schema("&1Am11aat2s^1A").is_ok());
    }

    #[test]
    fn test_sibling_back_reference_is_rejected() {
        // The declaration's scope ends with its body; a later sibling
        // cannot refer to it.
        let err = parse_schema("t2&1Am11x?^1A^1A").unwrap_err().to_string();
        assert!(err.contains("undeclared"), "{err}");
    }

    #[test]
    fn test_nesting_depth_is_capped() {
        let deep = format!("{}i4", "a".repeat(100_000));
        let err = parse_schema(&deep).unwrap_err().to_string();
        assert!(err.contains("nest"), "{err}");
        let ok = format!("{}i4", "a".repeat(MAX_SCHEMA_DEPTH - 1));
        assert!(parse_schema(&ok).is_ok());
        let just_over = format!("{}i4", "a".repeat(MAX_SCHEMA_DEPTH + 1));
        assert!(parse_schema(&just_over).is_err());
    }

    #[test]
    fn test_nested_same_name_declaration_keeps_its_own_width() {
        // An inner declaration reusing an outer name shadows it: the inner
        // back-reference resolves to the inner body, so its width must be
        // the inner width (24, a Str plus a pointer), not the outer (16).
        let s = parse_schema("&2T7m21vi44next?&2T7m21vs4next?^2T7").unwrap();
        let inner_decl = &s.parameters[1].parameters[0];
        assert_eq!(inner_decl.name.as_deref(), Some("T7"));
        assert_eq!(inner_decl.width, 24);
        let inner_recur = &inner_decl.parameters[1].parameters[0];
        assert_eq!(inner_recur.serial_type, SerialType::Recur);
        assert_eq!(inner_recur.width, 24);
        assert_eq!(s.width, 16);
    }
}

#[cfg(test)]
mod compat_tests {
    use super::*;

    #[test]
    fn test_schema_compat_with_c() {
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

        let t = parse_schema("t2i4s").unwrap();
        assert_eq!(t.offsets, vec![0, 8], "t2i4s offsets");

        let o = parse_schema("?i4").unwrap();
        assert!(o.offsets.is_empty(), "?i4 offsets");
        let os = parse_schema("?s").unwrap();
        assert!(os.offsets.is_empty(), "?s offsets");

        let s = parse_schema("s").unwrap();
        assert_eq!(s.parameters.len(), 1);
        assert_eq!(s.parameters[0].serial_type, SerialType::Uint8);
        assert_eq!(s.parameters[0].width, 1);
    }

}
