//! Delimited-text tables: the tokenizer, the header rules, the cell
//! grammar and the cell writer shared by every table read and write.
//! The rules are model/language/arguments-and-output.md CLI-13..CLI-17.

use std::borrow::Cow;

use crate::schema::{Schema, SerialType};

/// Delimiters tried on a header, in order (CLI-15).
pub const CANDIDATES: [u8; 2] = [b'\t', b','];

#[derive(Debug, Clone)]
pub struct Cell<'a> {
    pub text: Cow<'a, [u8]>,
    pub quoted: bool,
}

#[derive(Debug)]
pub struct Record<'a> {
    /// Line on which the record starts, counting from 1.
    pub line: usize,
    pub cells: Vec<Cell<'a>>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum End {
    Delim,
    Eol,
    Eof,
}

/// Splits delimited text into records under CLI-16's quoting rules.
pub struct Tokenizer<'a> {
    data: &'a [u8],
    pos: usize,
    line: usize,
    delim: u8,
}

impl<'a> Tokenizer<'a> {
    pub fn new(data: &'a [u8], pos: usize, line: usize, delim: u8) -> Tokenizer<'a> {
        Tokenizer { data, pos, line, delim }
    }

    /// Byte offset of the next record.
    pub fn pos(&self) -> usize {
        self.pos
    }

    /// Line of the next record.
    pub fn line(&self) -> usize {
        self.line
    }

    pub fn next_record(&mut self) -> Result<Option<Record<'a>>, String> {
        if self.pos >= self.data.len() {
            return Ok(None);
        }
        let line = self.line;
        let mut cells = Vec::new();
        loop {
            let (cell, end) = self.field()?;
            cells.push(cell);
            if end != End::Delim {
                break;
            }
        }
        Ok(Some(Record { line, cells }))
    }

    fn field(&mut self) -> Result<(Cell<'a>, End), String> {
        let d = self.data;
        if self.pos < d.len() && d[self.pos] == b'"' {
            let open_line = self.line;
            self.pos += 1;
            let mut seg = self.pos;
            let mut owned: Option<Vec<u8>> = None;
            loop {
                let q = match d[self.pos..].iter().position(|&b| b == b'"') {
                    Some(r) => self.pos + r,
                    None => {
                        return Err(format!(
                            "line {}: a quoted cell is never closed",
                            open_line
                        ))
                    }
                };
                self.line += count_lf(&d[self.pos..q]);
                if q + 1 < d.len() && d[q + 1] == b'"' {
                    owned.get_or_insert_with(Vec::new).extend_from_slice(&d[seg..=q]);
                    self.pos = q + 2;
                    seg = self.pos;
                    continue;
                }
                let text = match owned {
                    Some(mut o) => {
                        o.extend_from_slice(&d[seg..q]);
                        Cow::Owned(o)
                    }
                    None => Cow::Borrowed(&d[seg..q]),
                };
                self.pos = q + 1;
                let end = self.terminator().ok_or_else(|| {
                    format!(
                        "line {}: text follows a closing quote; a quoted cell must end at a delimiter or the end of its line",
                        self.line
                    )
                })?;
                return Ok((Cell { text, quoted: true }, end));
            }
        }
        let start = self.pos;
        let delim = self.delim;
        let stop = d[start..]
            .iter()
            .position(|&b| b == delim || b == b'\n' || b == b'\r')
            .map_or(d.len(), |r| start + r);
        self.pos = stop;
        let end = self.terminator().ok_or_else(|| {
            format!(
                "line {}: a carriage return that does not end the line",
                self.line
            )
        })?;
        Ok((Cell { text: Cow::Borrowed(&d[start..stop]), quoted: false }, end))
    }

    fn terminator(&mut self) -> Option<End> {
        let d = self.data;
        if self.pos >= d.len() {
            return Some(End::Eof);
        }
        let b = d[self.pos];
        if b == self.delim {
            self.pos += 1;
            Some(End::Delim)
        } else if b == b'\n' {
            self.pos += 1;
            self.line += 1;
            Some(End::Eol)
        } else if b == b'\r' && self.pos + 1 < d.len() && d[self.pos + 1] == b'\n' {
            self.pos += 2;
            self.line += 1;
            Some(End::Eol)
        } else {
            None
        }
    }
}

/// `n` cells, in words.
pub fn count_cells(n: usize) -> String {
    if n == 1 { "1 cell".into() } else { format!("{} cells", n) }
}

fn count_lf(s: &[u8]) -> usize {
    s.iter().filter(|&&b| b == b'\n').count()
}

/// What a header must name (CLI-14).
pub enum Expect<'s> {
    /// Exactly these names, in any order: the fields of a record.
    Exact(&'s [String]),
    /// At least these names: the declared columns of a table.
    Contains(&'s [String]),
    /// Anything: a table with no declared columns.
    Open,
}

#[derive(Debug)]
pub struct Header {
    pub delim: u8,
    pub names: Vec<String>,
    /// Offset of the first data record.
    pub body: usize,
    /// Line of the first data record.
    pub body_line: usize,
}

const UTF8_BOM: &[u8] = b"\xEF\xBB\xBF";

/// True when `data`, past any UTF-8 byte-order mark and leading ASCII
/// space, opens with `[` or `{`: the input is JSON, not a table's header
/// (CLI-15).
pub fn looks_like_json(data: &[u8]) -> bool {
    let data = data.strip_prefix(UTF8_BOM).unwrap_or(data);
    matches!(data.iter().find(|b| !b.is_ascii_whitespace()), Some(b'[') | Some(b'{'))
}

/// Find the delimiter and the column names from the first record
/// (CLI-15), checking the names against `expect` (CLI-14).
pub fn read_header(data: &[u8], expect: Expect<'_>) -> Result<Header, String> {
    if data.starts_with(b"\xFF\xFE") || data.starts_with(b"\xFE\xFF") {
        return Err("the input is UTF-16 text; a table must be UTF-8 (in Excel, save as \"CSV UTF-8\")".into());
    }
    let start = if data.starts_with(UTF8_BOM) { UTF8_BOM.len() } else { 0 };
    if start >= data.len() {
        return Err("the input is empty; a table needs a header row naming its columns".into());
    }
    let mut splits: Vec<(u8, Result<(Vec<String>, usize, usize), String>)> = Vec::new();
    for &delim in CANDIDATES.iter() {
        splits.push((delim, split_header(data, start, delim)));
    }
    let chosen = match &expect {
        Expect::Exact(want) => splits.iter().position(|(_, r)| match r {
            Ok((names, _, _)) => same_set(names, want),
            Err(_) => false,
        }),
        Expect::Contains(want) => splits.iter().position(|(_, r)| match r {
            Ok((names, _, _)) => {
                let have: std::collections::HashSet<&String> = names.iter().collect();
                want.iter().all(|w| have.contains(w))
            }
            Err(_) => false,
        }),
        Expect::Open => splits
            .iter()
            .position(|(_, r)| matches!(r, Ok((names, _, _)) if names.len() > 1))
            .or(Some(0)),
    };
    if let Some(i) = chosen {
        let (delim, r) = splits.swap_remove(i);
        let (names, body, body_line) = r?;
        check_names(&names)?;
        return Ok(Header { delim, names, body, body_line });
    }
    Err(header_mismatch(&splits, &expect))
}

fn split_header(data: &[u8], start: usize, delim: u8) -> Result<(Vec<String>, usize, usize), String> {
    let mut tok = Tokenizer::new(data, start, 1, delim);
    let rec = tok
        .next_record()?
        .ok_or_else(|| "the input has no header row".to_string())?;
    let mut names = Vec::with_capacity(rec.cells.len());
    for c in rec.cells {
        let s = std::str::from_utf8(&c.text)
            .map_err(|_| "line 1: the header is not valid UTF-8".to_string())?;
        names.push(s.to_string());
    }
    Ok((names, tok.pos(), tok.line()))
}

fn same_set(names: &[String], want: &[String]) -> bool {
    let have: std::collections::HashSet<&String> = names.iter().collect();
    names.len() == want.len() && want.iter().all(|w| have.contains(w))
}

pub fn check_names(names: &[String]) -> Result<(), String> {
    if names.iter().any(|n| n.contains('\0')) {
        return Err("line 1: the header holds a NUL byte (is the file UTF-16 without a byte-order mark?)".into());
    }
    if let Some(i) = names.iter().position(|n| n.is_empty()) {
        return Err(format!(
            "line 1: column {} has no name{}",
            i + 1,
            if i == 0 {
                " (an unnamed first column is usually a row index: write the file with index=False in pandas or row.names=FALSE in R)"
            } else if i + 1 == names.len() {
                " (a header that ends with a delimiter has an empty last column)"
            } else {
                ""
            }
        ));
    }
    let mut seen = std::collections::HashSet::with_capacity(names.len());
    for n in names {
        if !seen.insert(n.as_str()) {
            return Err(format!("line 1: column `{}` is named twice", n));
        }
    }
    Ok(())
}

/// The error for a header that matches under no candidate delimiter,
/// described under the split that looks most like the intended one.
fn header_mismatch(
    splits: &[(u8, Result<(Vec<String>, usize, usize), String>)],
    expect: &Expect<'_>,
) -> String {
    let best = splits
        .iter()
        .filter_map(|(d, r)| r.as_ref().ok().map(|(n, _, _)| (*d, n)))
        .max_by_key(|(_, n)| n.len());
    let (delim, names) = match best {
        Some(b) => b,
        None => {
            return splits
                .iter()
                .find_map(|(_, r)| r.as_ref().err().cloned())
                .unwrap_or_else(|| "the header row cannot be read".into())
        }
    };
    if let Err(e) = check_names(names) {
        return e;
    }
    let want: &[String] = match expect {
        Expect::Exact(w) | Expect::Contains(w) => w,
        Expect::Open => &[],
    };
    let missing: Vec<&String> = want.iter().filter(|w| !names.contains(w)).collect();
    let unexpected: Vec<&String> = match expect {
        Expect::Exact(_) => names.iter().filter(|n| !want.contains(n)).collect(),
        Expect::Contains(_) | Expect::Open => Vec::new(),
    };
    let mut msg = format!(
        "line 1: the header ({}) does not match the expected columns ({})",
        names.join(delim_name(delim)),
        want.join(", ")
    );
    if !missing.is_empty() {
        msg.push_str(&format!("; missing: {}", join_ticked(&missing)));
    }
    if !unexpected.is_empty() {
        msg.push_str(&format!("; not a field: {}", join_ticked(&unexpected)));
    }
    for m in &missing {
        if let Some(n) = names.iter().find(|n| n.eq_ignore_ascii_case(m) && n != m) {
            msg.push_str(&format!("; `{}` differs from `{}` only in case", n, m));
        } else if let Some(n) = names.iter().find(|n| n.trim() == m.as_str() && n != m) {
            msg.push_str(&format!("; `{}` has spaces around `{}`", n, m));
        }
    }
    if names.len() == 1 && names[0].contains(';') {
        msg.push_str("; the header is separated by `;`, which is not a table delimiter (save the file with commas or tabs)");
    }
    msg
}

fn delim_name(d: u8) -> &'static str {
    if d == b'\t' { "<TAB>" } else { "," }
}

fn join_ticked(xs: &[&String]) -> String {
    xs.iter().map(|x| format!("`{}`", x)).collect::<Vec<_>>().join(", ")
}

/// The type of one column, as the cell grammar reads it (CLI-16).
#[derive(Debug, Clone, PartialEq)]
pub enum CellType {
    Str,
    Bool,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    /// Arbitrary-precision `Int`.
    Int,
    /// `Int` in a table column, held as 64 bits.
    TableInt,
    F32,
    F64,
    Enum(Vec<String>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ColType {
    pub ty: CellType,
    pub optional: bool,
}

impl ColType {
    /// The column type of a field schema, or `None` if a cell cannot
    /// hold it.
    pub fn from_schema(s: &Schema) -> Option<ColType> {
        let (inner, optional) = if s.serial_type == SerialType::Optional {
            (s.parameters.first()?, true)
        } else {
            (s, false)
        };
        let ty = match inner.serial_type {
            SerialType::String => CellType::Str,
            SerialType::Bool => CellType::Bool,
            SerialType::Sint8 => CellType::I8,
            SerialType::Sint16 => CellType::I16,
            SerialType::Sint32 => CellType::I32,
            SerialType::Sint64 => CellType::I64,
            SerialType::Uint8 => CellType::U8,
            SerialType::Uint16 => CellType::U16,
            SerialType::Uint32 => CellType::U32,
            SerialType::Uint64 => CellType::U64,
            SerialType::Int => CellType::Int,
            SerialType::Float32 => CellType::F32,
            SerialType::Float64 => CellType::F64,
            SerialType::Enum => CellType::Enum(inner.keys.clone()),
            SerialType::Nil | SerialType::Array | SerialType::Tuple | SerialType::Map
            | SerialType::Optional | SerialType::Table | SerialType::Recur
            | SerialType::IFile | SerialType::OStream | SerialType::IStream
            | SerialType::Variant => return None,
        };
        Some(ColType { ty, optional })
    }

    pub fn name(&self) -> String {
        let base = match &self.ty {
            CellType::Str => "Str".to_string(),
            CellType::Bool => "Bool".to_string(),
            CellType::I8 => "I8".to_string(),
            CellType::I16 => "I16".to_string(),
            CellType::I32 => "I32".to_string(),
            CellType::I64 => "I64".to_string(),
            CellType::U8 => "U8".to_string(),
            CellType::U16 => "U16".to_string(),
            CellType::U32 => "U32".to_string(),
            CellType::U64 => "U64".to_string(),
            CellType::Int => "Int".to_string(),
            CellType::TableInt => "Int (64-bit in a table)".to_string(),
            CellType::F32 => "F32".to_string(),
            CellType::F64 => "Real".to_string(),
            CellType::Enum(keys) => format!("one of {}", keys.join(", ")),
        };
        if self.optional { format!("?{}", base) } else { base }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'a> {
    Null,
    Str(Cow<'a, str>),
    Bool(bool),
    Signed(i64),
    Unsigned(u64),
    /// Decimal digits of an `Int`, with an optional leading `-` and no
    /// leading zeros.
    Big(String),
    F64(f64),
    F32(f32),
    Enum(usize),
}

/// Read one cell as `ct` (CLI-16). The error names the cell and the
/// type; the caller adds the line and column.
pub fn parse_cell<'a>(cell: Cell<'a>, ct: &ColType) -> Result<Value<'a>, String> {
    if !cell.quoted && cell.text.is_empty() {
        return if ct.optional {
            Ok(Value::Null)
        } else {
            Err(format!(
                "an empty cell is null, but the column is {}, not optional",
                ct.name()
            ))
        };
    }
    let text: Cow<'a, str> = match cell.text {
        Cow::Borrowed(b) => Cow::Borrowed(std::str::from_utf8(b).map_err(|_| "the cell is not valid UTF-8".to_string())?),
        Cow::Owned(b) => Cow::Owned(String::from_utf8(b).map_err(|_| "the cell is not valid UTF-8".to_string())?),
    };
    let bad = |t: &str| format!("\"{}\" is not {}", shorten(t), ct.name());
    let t: &str = &text;
    let v = match &ct.ty {
        CellType::Str => {
            if t.contains('\0') {
                return Err("the cell holds a NUL byte".into());
            }
            return Ok(Value::Str(text));
        }
        CellType::Bool => {
            if t.eq_ignore_ascii_case("true") {
                Value::Bool(true)
            } else if t.eq_ignore_ascii_case("false") {
                Value::Bool(false)
            } else {
                return Err(bad(t));
            }
        }
        CellType::I8 => Value::Signed(signed(t, i8::MIN as i64, i8::MAX as i64).ok_or_else(|| bad(t))?),
        CellType::I16 => Value::Signed(signed(t, i16::MIN as i64, i16::MAX as i64).ok_or_else(|| bad(t))?),
        CellType::I32 => Value::Signed(signed(t, i32::MIN as i64, i32::MAX as i64).ok_or_else(|| bad(t))?),
        CellType::I64 | CellType::TableInt => Value::Signed(signed(t, i64::MIN, i64::MAX).ok_or_else(|| bad(t))?),
        CellType::U8 => Value::Unsigned(unsigned(t, u8::MAX as u64).ok_or_else(|| bad(t))?),
        CellType::U16 => Value::Unsigned(unsigned(t, u16::MAX as u64).ok_or_else(|| bad(t))?),
        CellType::U32 => Value::Unsigned(unsigned(t, u32::MAX as u64).ok_or_else(|| bad(t))?),
        CellType::U64 => Value::Unsigned(unsigned(t, u64::MAX).ok_or_else(|| bad(t))?),
        CellType::Int => Value::Big(big(t).ok_or_else(|| bad(t))?),
        CellType::F64 => Value::F64(float(t).ok_or_else(|| bad(t))?),
        CellType::F32 => Value::F32(float32(t).ok_or_else(|| bad(t))?),
        CellType::Enum(keys) => Value::Enum(keys.iter().position(|k| k == t).ok_or_else(|| bad(t))?),
    };
    Ok(v)
}

fn shorten(t: &str) -> String {
    if t.chars().count() <= 40 {
        return t.to_string();
    }
    let mut s: String = t.chars().take(37).collect();
    s.push_str("...");
    s
}

fn is_digits(s: &[u8]) -> bool {
    !s.is_empty() && s.iter().all(|b| b.is_ascii_digit())
}

fn is_int_text(t: &str) -> bool {
    is_digits(t.strip_prefix('-').unwrap_or(t).as_bytes())
}

fn signed(t: &str, lo: i64, hi: i64) -> Option<i64> {
    if !is_int_text(t) {
        return None;
    }
    let v: i64 = t.parse().ok()?;
    (v >= lo && v <= hi).then_some(v)
}

fn unsigned(t: &str, hi: u64) -> Option<u64> {
    if !is_digits(t.as_bytes()) {
        return None;
    }
    let v: u64 = t.parse().ok()?;
    (v <= hi).then_some(v)
}

fn big(t: &str) -> Option<String> {
    if !is_int_text(t) {
        return None;
    }
    let (neg, digits) = match t.strip_prefix('-') {
        Some(d) => (true, d),
        None => (false, t),
    };
    let digits = digits.trim_start_matches('0');
    Some(if digits.is_empty() {
        "0".to_string()
    } else if neg {
        format!("-{}", digits)
    } else {
        digits.to_string()
    })
}

/// `-?D+(.D+)?([eE][+-]?D+)?`, or `nan`, `inf`, `-inf`.
fn is_float_text(t: &str) -> bool {
    let b = t.strip_prefix('-').unwrap_or(t).as_bytes();
    let (mant, exp) = match b.iter().position(|&c| c == b'e' || c == b'E') {
        Some(i) => (&b[..i], Some(&b[i + 1..])),
        None => (b, None),
    };
    let mant_ok = match mant.iter().position(|&c| c == b'.') {
        Some(i) => is_digits(&mant[..i]) && is_digits(&mant[i + 1..]),
        None => is_digits(mant),
    };
    let exp_ok = match exp {
        None => true,
        Some(e) => {
            let e = e.strip_prefix(b"+").or_else(|| e.strip_prefix(b"-")).unwrap_or(e);
            is_digits(e)
        }
    };
    mant_ok && exp_ok
}

fn nonfinite(t: &str) -> Option<f64> {
    match t {
        "nan" => Some(f64::NAN),
        "inf" => Some(f64::INFINITY),
        "-inf" => Some(f64::NEG_INFINITY),
        _ => None,
    }
}

fn float(t: &str) -> Option<f64> {
    if let Some(v) = nonfinite(t) {
        return Some(v);
    }
    if !is_float_text(t) {
        return None;
    }
    let v: f64 = t.parse().ok()?;
    v.is_finite().then_some(v)
}

fn float32(t: &str) -> Option<f32> {
    if let Some(v) = nonfinite(t) {
        #[allow(clippy::cast_possible_truncation)]
        return Some(v as f32);
    }
    if !is_float_text(t) {
        return None;
    }
    let v: f32 = t.parse().ok()?;
    v.is_finite().then_some(v)
}

/// Append `v` as the JSON the value loader reads for `ct`.
pub fn push_json(out: &mut String, v: &Value<'_>, ct: &ColType) {
    match v {
        Value::Null => out.push_str("null"),
        Value::Str(s) => out.push_str(&serde_json::to_string(s.as_ref()).unwrap_or_else(|_| "\"\"".into())),
        Value::Bool(b) => out.push_str(if *b { "true" } else { "false" }),
        Value::Signed(i) => out.push_str(&i.to_string()),
        Value::Unsigned(u) => out.push_str(&u.to_string()),
        Value::Big(d) => out.push_str(d),
        Value::F64(f) => push_float_json(out, *f, false),
        Value::F32(f) => push_float_json(out, f64::from(*f), true),
        Value::Enum(i) => {
            if let CellType::Enum(keys) = &ct.ty {
                out.push_str(&serde_json::to_string(&keys[*i]).unwrap_or_else(|_| "\"\"".into()));
            }
        }
    }
}

fn push_float_json(out: &mut String, f: f64, single: bool) {
    if f.is_nan() {
        out.push_str("\"nan\"");
    } else if f.is_infinite() {
        out.push_str(if f > 0.0 { "\"inf\"" } else { "\"-inf\"" });
    } else {
        out.push_str(std::str::from_utf8(crate::json::FloatText::new(f, single).as_bytes()).unwrap_or("0"));
    }
}

/// Append a float cell in the spelling the reader accepts (CLI-17).
pub fn push_float_cell(out: &mut Vec<u8>, f: f64, single: bool) {
    if f.is_nan() {
        out.extend_from_slice(b"nan");
    } else if f.is_infinite() {
        out.extend_from_slice(if f > 0.0 { b"inf" } else { b"-inf" });
    } else {
        out.extend_from_slice(crate::json::FloatText::new(f, single).as_bytes());
    }
}

/// Append a non-null text cell, quoted where the reader needs it to be
/// (CLI-19): an empty cell, one holding a candidate delimiter, a quote, a
/// carriage return or a line feed, or one that opens like JSON. Both
/// candidates are quoted, whatever `delim` is, because the reader chooses
/// the delimiter from the header.
pub fn push_text_cell(out: &mut Vec<u8>, text: &[u8], delim: u8) {
    let opens_like_json = matches!(text.iter().find(|b| !b.is_ascii_whitespace()), Some(b'[') | Some(b'{'));
    let needs = text.is_empty()
        || opens_like_json
        || text.iter().any(|&b| b == delim || CANDIDATES.contains(&b) || b == b'"' || b == b'\n' || b == b'\r');
    if !needs {
        out.extend_from_slice(text);
        return;
    }
    out.push(b'"');
    for &b in text {
        if b == b'"' {
            out.push(b'"');
        }
        out.push(b);
    }
    out.push(b'"');
}

/// The column types of a record's fields, in field order.
pub fn field_types(row: &Schema) -> Result<Vec<ColType>, String> {
    row.keys
        .iter()
        .zip(row.parameters.iter())
        .map(|(k, p)| {
            ColType::from_schema(p).ok_or_else(|| format!("field `{}` cannot be read from a table cell", k))
        })
        .collect()
}

/// For each column, the index of the record field it names. The names
/// must be exactly the fields (CLI-14).
pub fn field_index(names: &[String], row: &Schema) -> Result<Vec<usize>, String> {
    check_names(names)?;
    let field: std::collections::HashMap<&str, usize> =
        row.keys.iter().enumerate().map(|(i, k)| (k.as_str(), i)).collect();
    let mut idx = Vec::with_capacity(names.len());
    for n in names {
        match field.get(n.as_str()) {
            Some(&i) => idx.push(i),
            None => return Err(format!("column `{}` is not a field of the record ({})", n, row.keys.join(", "))),
        }
    }
    if idx.len() != row.keys.len() {
        if let Some(k) = row.keys.iter().find(|k| !names.contains(k)) {
            return Err(format!("the record field `{}` has no column", k));
        }
    }
    Ok(idx)
}

/// Read a headed delimited table as a list of records `row`, written as
/// the JSON array the value loader reads.
pub fn rows_to_json(data: &[u8], row: &Schema) -> Result<String, String> {
    let types = field_types(row)?;
    let header = read_header(data, Expect::Exact(&row.keys))?;
    let idx = field_index(&header.names, row)?;
    let ncol = header.names.len();
    let mut tok = Tokenizer::new(data, header.body, header.body_line, header.delim);
    let mut parts: Vec<String> = vec![String::new(); ncol];
    let mut out = String::with_capacity(data.len() + data.len() / 2);
    out.push('[');
    let mut first = true;
    while let Some(rec) = tok.next_record()? {
        if rec.cells.len() != ncol {
            return Err(format!(
                "line {}: {}, but the header names {} columns",
                rec.line,
                count_cells(rec.cells.len()),
                ncol
            ));
        }
        for (j, c) in rec.cells.into_iter().enumerate() {
            let f = idx[j];
            let v = parse_cell(c, &types[f])
                .map_err(|e| format!("line {}, column `{}`: {}", rec.line, header.names[j], e))?;
            parts[f].clear();
            push_json(&mut parts[f], &v, &types[f]);
        }
        if !first {
            out.push(',');
        }
        first = false;
        out.push('{');
        for (f, k) in row.keys.iter().enumerate() {
            if f > 0 {
                out.push(',');
            }
            out.push_str(&serde_json::to_string(k).unwrap_or_else(|_| "\"\"".into()));
            out.push(':');
            out.push_str(&parts[f]);
        }
        out.push('}');
    }
    out.push(']');
    Ok(out)
}

/// Write a list of records `row`, given as the JSON the value writer
/// produces, as a headed delimited table (CLI-17).
pub fn rows_from_json(json: &str, row: &Schema, delim: u8) -> Result<Vec<u8>, String> {
    use serde_json::value::RawValue;
    let types = field_types(row)?;
    let rows: Vec<std::collections::HashMap<String, Box<RawValue>>> =
        serde_json::from_str(json).map_err(|e| format!("reading the records to write: {}", e))?;
    let mut out = Vec::with_capacity(json.len());
    for (i, k) in row.keys.iter().enumerate() {
        if i > 0 {
            out.push(delim);
        }
        push_text_cell(&mut out, k.as_bytes(), delim);
    }
    out.push(b'\n');
    for r in &rows {
        for (i, (k, t)) in row.keys.iter().zip(types.iter()).enumerate() {
            if i > 0 {
                out.push(delim);
            }
            let raw = r.get(k).map(|v| v.get().trim()).ok_or_else(|| format!("a record has no field `{}`", k))?;
            if raw == "null" {
                continue;
            }
            match &t.ty {
                CellType::Str | CellType::Enum(_) => {
                    let s: String = serde_json::from_str(raw).map_err(|e| format!("field `{}`: {}", k, e))?;
                    push_text_cell(&mut out, s.as_bytes(), delim);
                }
                CellType::F32 | CellType::F64 => {
                    out.extend_from_slice(raw.trim_matches('"').as_bytes());
                }
                CellType::Bool | CellType::I8 | CellType::I16 | CellType::I32 | CellType::I64
                | CellType::U8 | CellType::U16 | CellType::U32 | CellType::U64 | CellType::Int
                | CellType::TableInt => out.extend_from_slice(raw.as_bytes()),
            }
        }
        out.push(b'\n');
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn records(data: &str, delim: u8) -> Result<Vec<Vec<(String, bool)>>, String> {
        let mut t = Tokenizer::new(data.as_bytes(), 0, 1, delim);
        let mut out = Vec::new();
        while let Some(r) = t.next_record()? {
            out.push(
                r.cells
                    .into_iter()
                    .map(|c| (String::from_utf8(c.text.into_owned()).unwrap(), c.quoted))
                    .collect(),
            );
        }
        Ok(out)
    }

    fn texts(data: &str, delim: u8) -> Vec<Vec<String>> {
        records(data, delim).unwrap().into_iter().map(|r| r.into_iter().map(|c| c.0).collect()).collect()
    }

    #[test]
    fn tokenizer_splits_quotes_and_line_ends() {
        assert_eq!(texts("a,b\n1,2\n", b','), vec![vec!["a", "b"], vec!["1", "2"]]);
        assert_eq!(texts("a,b\r\n1,2", b','), vec![vec!["a", "b"], vec!["1", "2"]]);
        assert_eq!(texts("\"x, y\",\"say \"\"hi\"\"\"\n", b','), vec![vec!["x, y", "say \"hi\""]]);
        assert_eq!(texts("\"two\nlines\",z\n", b','), vec![vec!["two\nlines", "z"]]);
        assert_eq!(texts("a\t\"b\tc\"\n", b'\t'), vec![vec!["a", "b\tc"]]);
        assert_eq!(texts("say \"hi\",2\n", b','), vec![vec!["say \"hi\"", "2"]]);
        assert_eq!(texts("a,\n", b','), vec![vec!["a", ""]]);
        assert_eq!(texts("a\n\nb\n", b','), vec![vec!["a"], vec![""], vec!["b"]]);
    }

    #[test]
    fn tokenizer_reports_quotedness() {
        let r = records("\"\",\n", b',').unwrap();
        assert_eq!(r[0], vec![("".to_string(), true), ("".to_string(), false)]);
    }

    #[test]
    fn tokenizer_rejects_malformed_quotes() {
        assert!(records("\"Best\" gene,1\n", b',').is_err());
        assert!(records("\"open,1\n", b',').is_err());
        assert!(records("a\rb\n", b',').is_err());
    }

    #[test]
    fn tokenizer_counts_lines_through_quoted_newlines() {
        let mut t = Tokenizer::new(b"\"a\nb\",1\nc,2\n", 0, 1, b',');
        assert_eq!(t.next_record().unwrap().unwrap().line, 1);
        assert_eq!(t.next_record().unwrap().unwrap().line, 3);
    }

    fn names(xs: &[&str]) -> Vec<String> {
        xs.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn header_picks_the_delimiter_that_names_the_fields() {
        let want = names(&["name", "count"]);
        let h = read_header(b"count,name\nx,1\n", Expect::Exact(&want)).unwrap();
        assert_eq!(h.delim, b',');
        assert_eq!(h.names, names(&["count", "name"]));
        assert_eq!(h.body, 11);
        assert_eq!(h.body_line, 2);
        let h = read_header(b"name\tcount\n", Expect::Exact(&want)).unwrap();
        assert_eq!(h.delim, b'\t');
        let h = read_header(b"\xEF\xBB\xBF\"name\",\"count\"\n", Expect::Exact(&want)).unwrap();
        assert_eq!(h.delim, b',');
    }

    #[test]
    fn header_mismatch_is_an_error() {
        let want = names(&["name", "count"]);
        assert!(read_header(b"name\n", Expect::Exact(&want)).is_err());
        assert!(read_header(b"name,count,extra\n", Expect::Exact(&want)).is_err());
        assert!(read_header(b"name,Count\n", Expect::Exact(&want)).unwrap_err().contains("only in case"));
        assert!(read_header(b"name;count\n", Expect::Exact(&want)).unwrap_err().contains("`;`"));
        assert!(read_header(b"", Expect::Exact(&want)).is_err());
        assert!(read_header(b"\xFF\xFEn\0", Expect::Exact(&want)).is_err());
        let one = names(&["name"]);
        assert!(read_header(b"name,name\n", Expect::Contains(&one)).unwrap_err().contains("twice"));
        assert!(read_header(b",name\n", Expect::Contains(&one)).unwrap_err().contains("row index"));
    }

    #[test]
    fn header_contains_keeps_extra_columns() {
        let want = names(&["x"]);
        let h = read_header(b"y,x,z\n", Expect::Contains(&want)).unwrap();
        assert_eq!(h.names, names(&["y", "x", "z"]));
    }

    #[test]
    fn open_header_prefers_a_split_with_several_columns() {
        assert_eq!(read_header(b"a,b,c\n", Expect::Open).unwrap().delim, b',');
        assert_eq!(read_header(b"a\tb,c\n", Expect::Open).unwrap().delim, b'\t');
        let h = read_header(b"only\n", Expect::Open).unwrap();
        assert_eq!(h.delim, b'\t');
        assert_eq!(h.names, names(&["only"]));
    }

    fn cell(t: &str, quoted: bool) -> Cell<'_> {
        Cell { text: Cow::Borrowed(t.as_bytes()), quoted }
    }

    fn ct(ty: CellType, optional: bool) -> ColType {
        ColType { ty, optional }
    }

    #[test]
    fn empty_cells_are_null() {
        assert_eq!(parse_cell(cell("", false), &ct(CellType::I64, true)), Ok(Value::Null));
        assert!(parse_cell(cell("", false), &ct(CellType::I64, false)).is_err());
        assert!(parse_cell(cell("", false), &ct(CellType::Str, false)).is_err());
        assert_eq!(parse_cell(cell("", true), &ct(CellType::Str, true)), Ok(Value::Str("".into())));
        assert_eq!(parse_cell(cell("", false), &ct(CellType::Str, true)), Ok(Value::Null));
        assert!(parse_cell(cell("", true), &ct(CellType::I64, true)).is_err());
    }

    #[test]
    fn cell_grammar() {
        fn p(t: &str, ty: CellType) -> Result<Value<'_>, String> {
            parse_cell(cell(t, false), &ct(ty, false))
        }
        assert_eq!(p("TRUE", CellType::Bool), Ok(Value::Bool(true)));
        assert_eq!(p("False", CellType::Bool), Ok(Value::Bool(false)));
        assert!(p("1", CellType::Bool).is_err());
        assert!(p("yes", CellType::Bool).is_err());
        assert_eq!(p("-12", CellType::I64), Ok(Value::Signed(-12)));
        assert!(p("+12", CellType::I64).is_err());
        assert!(p(" 12", CellType::I64).is_err());
        assert!(p("1.0", CellType::I64).is_err());
        assert!(p("128", CellType::I8).is_err());
        assert!(p("-1", CellType::U8).is_err());
        assert_eq!(p("0042", CellType::Int), Ok(Value::Big("42".into())));
        assert_eq!(p("-000", CellType::Int), Ok(Value::Big("0".into())));
        assert_eq!(
            p("123456789012345678901234567890", CellType::Int),
            Ok(Value::Big("123456789012345678901234567890".into()))
        );
        assert!(p("99999999999999999999", CellType::TableInt).is_err());
        assert_eq!(p("1e+300", CellType::F64), Ok(Value::F64(1e300)));
        assert_eq!(p("2", CellType::F64), Ok(Value::F64(2.0)));
        assert!(matches!(p("nan", CellType::F64), Ok(Value::F64(f)) if f.is_nan()));
        assert_eq!(p("-inf", CellType::F64), Ok(Value::F64(f64::NEG_INFINITY)));
        assert!(p("NaN", CellType::F64).is_err());
        assert!(p(".5", CellType::F64).is_err());
        assert!(p("5.", CellType::F64).is_err());
        assert!(p("1e999", CellType::F64).is_err());
        assert!(p("Infinity", CellType::F64).is_err());
        assert_eq!(p("0.1", CellType::F32), Ok(Value::F32(0.1f32)));
        assert!(p("1e39", CellType::F32).is_err());
        let e = CellType::Enum(names(&["A", "B"]));
        assert_eq!(p("B", e.clone()), Ok(Value::Enum(1)));
        assert!(p("b", e).is_err());
        assert_eq!(p(" x ", CellType::Str), Ok(Value::Str(" x ".into())));
    }

    fn row(s: &str) -> Schema {
        crate::schema::parse_schema(s).unwrap()
    }

    #[test]
    fn rows_bind_columns_by_name() {
        let r = row("m24names1n?j");
        assert_eq!(
            rows_to_json(b"n,name\n1,a\n,b\n", &r).unwrap(),
            r#"[{"name":"a","n":1},{"name":"b","n":null}]"#
        );
        assert_eq!(rows_to_json(b"name\tn\n", &r).unwrap(), "[]");
    }

    #[test]
    fn rows_reject_what_they_do_not_define() {
        let r = row("m24names1n?j");
        assert!(rows_to_json(b"name,n\na,1,2\n", &r).unwrap_err().starts_with("line 2:"));
        assert!(rows_to_json(b"name,n\na,1\nb\n", &r).unwrap_err().starts_with("line 3:"));
        assert!(rows_to_json(b"name,n\na,x\n", &r).unwrap_err().contains("column `n`"));
        assert!(rows_to_json(b"name,n\n,1\n", &r).is_err());
        assert!(rows_to_json(b"name,n\na,1\n\n", &r).is_err());
        assert!(rows_to_json(b"a,1\n", &r).is_err());
        let all_str = row("m24names5chroms");
        assert!(rows_to_json(b"tp53,chr17\n", &all_str).is_err());
    }

    #[test]
    fn written_rows_read_back() {
        let r = row("m44names1n?j1wf81bb");
        let json = r#"[{"name":"","n":null,"w":"nan","b":true},{"name":"a,\"b\"","n":-12345678901234567890123,"w":1e+300,"b":false}]"#;
        for delim in CANDIDATES {
            let text = rows_from_json(json, &r, delim).unwrap();
            let back = rows_to_json(&text, &r).unwrap();
            assert_eq!(back, r#"[{"name":"","n":null,"w":"nan","b":true},{"name":"a,\"b\"","n":-12345678901234567890123,"w":1e+300,"b":false}]"#);
        }
    }

    #[test]
    fn a_single_column_round_trips_whatever_the_delimiter() {
        let r = row("m11ss");
        let json = r#"[{"s":"a\tb"},{"s":"c,d"}]"#;
        for delim in CANDIDATES {
            let text = rows_from_json(json, &r, delim).unwrap();
            assert_eq!(rows_to_json(&text, &r).unwrap(), json);
        }
    }

    #[test]
    fn a_name_that_opens_like_json_round_trips() {
        let r = row("m21[s1xj");
        let json = r#"[{"[":"{a}","x":1}]"#;
        for delim in CANDIDATES {
            let text = rows_from_json(json, &r, delim).unwrap();
            assert!(!looks_like_json(&text));
            assert_eq!(rows_to_json(&text, &r).unwrap(), json);
        }
    }

    #[test]
    fn json_behind_a_byte_order_mark_is_json() {
        assert!(looks_like_json(b"\xEF\xBB\xBF [{}]"));
        assert!(looks_like_json(b"\n{\"a\":1}"));
        assert!(!looks_like_json(b"\xEF\xBB\xBFa,b\n"));
        assert!(!looks_like_json(b"\"{a}\",b\n"));
    }

    #[test]
    fn nul_bytes_are_refused() {
        assert!(read_header(b"a\0,\0b\0\n", Expect::Open).unwrap_err().contains("NUL"));
        assert!(parse_cell(cell("a\0b", false), &ct(CellType::Str, false)).is_err());
    }

    #[test]
    fn quoted_numbers_read_as_numbers() {
        assert_eq!(parse_cell(cell("7", true), &ct(CellType::I64, false)), Ok(Value::Signed(7)));
    }

    #[test]
    fn written_cells_read_back() {
        for s in ["", "plain", "a,b", "say \"hi\"", "two\nlines", "\r", "\"lead"] {
            let mut out = Vec::new();
            push_text_cell(&mut out, s.as_bytes(), b',');
            out.push(b'\n');
            let r = records(std::str::from_utf8(&out).unwrap(), b',').unwrap();
            assert_eq!(r.len(), 1, "{:?}", s);
            let c = &r[0][0];
            let v = parse_cell(cell(&c.0, c.1), &ct(CellType::Str, true)).unwrap();
            assert_eq!(v, Value::Str(s.into()), "{:?}", s);
        }
        for f in [0.1, 1e300, -2.5e-7, 3.0, f64::INFINITY, f64::NEG_INFINITY] {
            let mut out = Vec::new();
            push_float_cell(&mut out, f, false);
            let t = String::from_utf8(out).unwrap();
            assert_eq!(parse_cell(cell(&t, false), &ct(CellType::F64, false)), Ok(Value::F64(f)), "{}", t);
        }
        let mut out = Vec::new();
        push_float_cell(&mut out, f64::from(0.1f32), true);
        let t = String::from_utf8(out).unwrap();
        assert_eq!(parse_cell(cell(&t, false), &ct(CellType::F32, false)), Ok(Value::F32(0.1f32)));
    }
}
