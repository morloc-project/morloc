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
    Tensor = 12,
    String = 13,
    Array = 14,
    Tuple = 15,
    Map = 16,
    Optional = 17,
}

/// Schema character codes for parsing schema strings.
const SCHEMA_NIL: u8 = b'z';
const SCHEMA_BOOL: u8 = b'b';
const SCHEMA_SINT: u8 = b'i';
const SCHEMA_UINT: u8 = b'u';
const SCHEMA_FLOAT: u8 = b'f';
const SCHEMA_STRING: u8 = b's';
const SCHEMA_ARRAY: u8 = b'a';
const SCHEMA_TENSOR: u8 = b'T';
const SCHEMA_TUPLE: u8 = b't';
const SCHEMA_MAP: u8 = b'm';
const SCHEMA_OPTIONAL: u8 = b'?';

/// Recursive schema definition, mirroring the C Schema struct.
#[derive(Debug, Clone)]
pub struct Schema {
    pub serial_type: SerialType,
    /// Number of parameters (e.g., array has 1, tuple has N).
    pub size: usize,
    /// Byte width when stored in a fixed-width array.
    pub width: usize,
    /// Field offsets for tuples/records, or ndim storage for tensors.
    pub offsets: Vec<usize>,
    /// Optional type hint string.
    pub hint: Option<String>,
    /// Child schemas (element type for arrays, field types for tuples, etc.).
    pub parameters: Vec<Schema>,
    /// Field names for records (None for non-record types).
    pub keys: Vec<String>,
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
            SerialType::String => std::mem::size_of::<shm::Array>(),
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
            SerialType::String | SerialType::Array | SerialType::Map | SerialType::Tensor => {
                std::mem::size_of::<usize>() // pointer-sized alignment
            }
            SerialType::Tuple => {
                self.parameters
                    .iter()
                    .map(|p| p.alignment())
                    .max()
                    .unwrap_or(1)
            }
            SerialType::Optional => {
                if let Some(inner) = self.parameters.first() {
                    std::cmp::max(1, inner.alignment())
                } else {
                    1
                }
            }
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
/// - `T2f8` -> 2D Tensor of Float64
/// - `<hint>i4` -> Sint32 with hint annotation
pub fn parse_schema(input: &str) -> Result<Schema, MorlocError> {
    let bytes = input.as_bytes();
    let (schema, consumed) = parse_schema_r(bytes, 0)?;
    if consumed != bytes.len() {
        return Err(MorlocError::Schema(format!(
            "trailing characters after schema at position {consumed}"
        )));
    }
    Ok(schema)
}

/// Recursive schema parser matching the C `parse_schema_r` format exactly.
fn parse_schema_r(bytes: &[u8], pos: usize) -> Result<(Schema, usize), MorlocError> {
    if pos >= bytes.len() {
        return Err(MorlocError::Schema("unexpected end of schema".into()));
    }

    let c = bytes[pos];
    let cur = pos + 1;

    match c {
        b'<' => {
            // Hint: <...> with nesting support, then parse the actual type
            let (hint, after_hint) = parse_hint(bytes, cur)?;
            let (mut schema, end) = parse_schema_r(bytes, after_hint)?;
            schema.hint = Some(hint);
            Ok((schema, end))
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
            }, cur))
        }
        SCHEMA_SINT => parse_sized_int(bytes, cur, true),
        SCHEMA_UINT => parse_sized_int(bytes, cur, false),
        SCHEMA_FLOAT => parse_sized_float(bytes, cur),
        SCHEMA_ARRAY => {
            // Array: one child schema follows immediately
            let (child, end) = parse_schema_r(bytes, cur)?;
            Ok((make_array_schema(child), end))
        }
        SCHEMA_OPTIONAL => {
            // Optional: one child schema follows immediately
            let (child, end) = parse_schema_r(bytes, cur)?;
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
                let (child, end) = parse_schema_r(bytes, p)?;
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
                let (child, end) = parse_schema_r(bytes, p)?;
                params.push(child);
                p = end;
            }
            Ok((make_map_schema(params, keys), p))
        }
        SCHEMA_TENSOR => {
            // Tensor: base-62 ndim char, then element schema
            if cur >= bytes.len() {
                return Err(MorlocError::Schema("expected tensor ndim".into()));
            }
            let ndim = decode_base62(bytes[cur])?;
            let (child, end) = parse_schema_r(bytes, cur + 1)?;
            Ok((make_tensor_schema(ndim, child), end))
        }
        _ => Err(MorlocError::Schema(format!(
            "unknown schema character '{}' at position {pos}",
            c as char
        ))),
    }
}

/// Parse hint with nested angle bracket support: `<std::vector<$1>>` etc.
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

// ── Schema constructors ────────────────────────────────────────────────────

fn make_array_schema(child: Schema) -> Schema {
    Schema {
        serial_type: SerialType::Array,
        size: 1,
        width: std::mem::size_of::<crate::shm::Array>(),
        offsets: Vec::new(),
        hint: None,
        parameters: vec![child],
        keys: Vec::new(),
    }
}

fn make_optional_schema(child: Schema) -> Schema {
    let align = child.alignment().max(1);
    let inner_offset = crate::shm::align_up(1, align);
    Schema {
        serial_type: SerialType::Optional,
        size: 1,
        width: inner_offset + child.width,
        offsets: vec![inner_offset],
        hint: None,
        parameters: vec![child],
        keys: Vec::new(),
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
    }
}

fn make_tensor_schema(ndim: usize, child: Schema) -> Schema {
    Schema {
        serial_type: SerialType::Tensor,
        size: 1,
        width: std::mem::size_of::<crate::shm::Tensor>(),
        offsets: vec![ndim],
        hint: None,
        parameters: vec![child],
        keys: Vec::new(),
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
        SerialType::Optional => {
            buf.push('?');
            schema_to_string_inner(&schema.parameters[0], buf);
        }
        SerialType::Tensor => {
            let ndim = schema.offsets.first().copied().unwrap_or(0);
            buf.push('T');
            buf.push(encode_base62(ndim));
            schema_to_string_inner(&schema.parameters[0], buf);
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
    fn test_parse_tensor() {
        let s = parse_schema("T2f8").unwrap();
        assert_eq!(s.serial_type, SerialType::Tensor);
        assert_eq!(s.offsets[0], 2); // ndim
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
        let cases = ["z", "b", "i4", "u8", "f8", "s", "ai4", "t2i4s", "?i4", "T2f8"];
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
            ("T2f8", "type=12 size=1 width=32"),
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

        // Verify tensor
        let t = parse_schema("T2f8").unwrap();
        assert_eq!(t.offsets, vec![2]); // ndim
        assert_eq!(t.width, 32); // sizeof(Tensor)
    }
}
