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
        use crate::shm;
        let width = match serial_type {
            SerialType::Nil => 0,
            SerialType::Bool | SerialType::Sint8 | SerialType::Uint8 => 1,
            SerialType::Sint16 | SerialType::Uint16 => 2,
            SerialType::Sint32 | SerialType::Uint32 | SerialType::Float32 => 4,
            SerialType::Sint64 | SerialType::Uint64 | SerialType::Float64 => 8,
            SerialType::String | SerialType::Int => std::mem::size_of::<shm::Array>(),
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
            SerialType::Tuple => self.parameters.iter().all(|p| p.is_fixed_width()),
            SerialType::Optional => false,
            // A Recur back-references a record whose layout includes
            // variable-length payload; never fixed-width.
            SerialType::Recur => false,
            _ => false,
        }
    }

    /// Alignment requirement for this type.
    pub fn alignment(&self) -> usize {
        match self.serial_type {
            SerialType::Nil => 1,
            SerialType::Bool | SerialType::Sint8 | SerialType::Uint8 => 1,
            SerialType::Sint16 | SerialType::Uint16 => 2,
            SerialType::Sint32 | SerialType::Uint32 | SerialType::Float32 => 4,
            SerialType::Sint64 | SerialType::Uint64 | SerialType::Float64 => 8,
            SerialType::String | SerialType::Array | SerialType::Map
            | SerialType::Int | SerialType::Table => {
                // Table values live in SHM as a single relative pointer
                // to an Arrow buffer; same pointer-sized alignment as
                // other indirect types.
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
                width: std::mem::size_of::<crate::shm::Array>(),
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
            let n = decode_base62(bytes[cur])?;
            let mut params = Vec::with_capacity(n);
            let mut p = cur + 1;
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
            let n = decode_base62(bytes[cur])?;
            let mut params = Vec::with_capacity(n);
            let mut keys = Vec::with_capacity(n);
            let mut p = cur + 1;
            for _ in 0..n {
                // Read key: base-62 length char + that many bytes
                if p >= bytes.len() {
                    return Err(MorlocError::Schema("expected map key length".into()));
                }
                let key_len = decode_base62(bytes[p])?;
                p += 1;
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
                let n = decode_base62(bytes[cur])?;
                let mut params = Vec::with_capacity(n);
                let mut keys = Vec::with_capacity(n);
                let mut p = cur + 1;
                for _ in 0..n {
                    if p >= bytes.len() {
                        return Err(MorlocError::Schema("expected table column key length".into()));
                    }
                    let key_len = decode_base62(bytes[p])?;
                    p += 1;
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
    let klen = decode_base62(bytes[pos])?;
    let start = pos + 1;
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
        _ => '\x07', // bell - error
    }
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
        width: std::mem::size_of::<crate::shm::Array>(),
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
        width: std::mem::size_of::<crate::shm::RelPtr>(),
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
fn make_table_schema(params: Vec<Schema>, keys: Vec<String>) -> Schema {
    Schema {
        serial_type: SerialType::Table,
        size: params.len(),
        width: std::mem::size_of::<crate::shm::Array>(),
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
        width: std::mem::size_of::<crate::shm::Array>(),
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

fn schema_to_string_inner(schema: &Schema, buf: &mut String) {
    // Write hint if present
    if let Some(ref hint) = schema.hint {
        buf.push('<');
        buf.push_str(hint);
        buf.push('>');
    }

    // Emit `&<klen><n>` for a named-schema declaration. Only the
    // declaration site carries `name` on a non-Recur node; `Recur`
    // nodes also carry `name` (the back-ref target) but are emitted as
    // `^<klen><n>` in their own arm below.
    if schema.serial_type != SerialType::Recur {
        if let Some(ref n) = schema.name {
            buf.push('&');
            buf.push(encode_base62(n.len()));
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
            buf.push(encode_base62(schema.size));
            for p in &schema.parameters {
                schema_to_string_inner(p, buf);
            }
        }
        SerialType::Map => {
            buf.push('m');
            buf.push(encode_base62(schema.size));
            for (i, p) in schema.parameters.iter().enumerate() {
                if i < schema.keys.len() {
                    let key = &schema.keys[i];
                    buf.push(encode_base62(key.len()));
                    buf.push_str(key);
                }
                schema_to_string_inner(p, buf);
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
            // list, `T:K<entries>` otherwise. Hint has already been
            // written above (Tables emit no hint by default, but the
            // round-trip preserves whatever was parsed).
            buf.push('T');
            if schema.size > 0 {
                buf.push(':');
                buf.push(encode_base62(schema.size));
                for (i, p) in schema.parameters.iter().enumerate() {
                    if i < schema.keys.len() {
                        let key = &schema.keys[i];
                        buf.push(encode_base62(key.len()));
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
                buf.push(encode_base62(n.len()));
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
        // These must match the C output exactly
        let cases = vec![
            ("s", "type=13 size=1 width=16"),
            ("ai4", "type=14 size=1 width=16"),
            ("t2i4s", "type=15 size=2 width=24"),
            ("?i4", "type=17 size=1 width=8"),
            ("?s", "type=17 size=1 width=24"),
        ];
        for (input, expected_root) in &cases {
            let s = parse_schema(input).unwrap();
            let got = format!("type={} size={} width={}", s.serial_type as u32, s.size, s.width);
            assert_eq!(&got, *expected_root, "Schema '{}' mismatch", input);
        }

        // Verify tuple offsets
        let t = parse_schema("t2i4s").unwrap();
        assert_eq!(t.offsets, vec![0, 8], "t2i4s offsets");

        // Verify optional offsets
        let o = parse_schema("?i4").unwrap();
        assert_eq!(o.offsets, vec![4], "?i4 offsets");
        let os = parse_schema("?s").unwrap();
        assert_eq!(os.offsets, vec![8], "?s offsets");

        // Verify string has uint8 parameter
        let s = parse_schema("s").unwrap();
        assert_eq!(s.parameters.len(), 1);
        assert_eq!(s.parameters[0].serial_type, SerialType::Uint8);
        assert_eq!(s.parameters[0].width, 1);
    }
}
