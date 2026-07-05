//! Rust-side parser and schema-check for morloc pattern chains
//! (`.[i]`, `.foo`, `.[start:stop:step]`, `.foo.[i:j].(.a;.b)`, ...).
//!
//! Mirrors `Morloc.PatternChain.Grammar` (Haskell) closely enough that
//! the same pattern string parses into the same shape on both sides.
//! Two extensions vs. the Haskell grammar surface:
//!
//! 1. **Literal bracket args.** The Haskell grammar accepts placeholder
//!    forms only (`.[]` for bracket-index, `.[:]` for bracket-slice)
//!    because in compile-time contexts the literal values live outside
//!    the pattern string (in surrounding expressions). The runtime
//!    walker (`mlc_ifile_walk`) takes literals separately via
//!    `IFileWalkArg[]`. For `morloc-nexus view --pattern` the user
//!    types a self-contained string like `.[101:200]`, so this parser
//!    also accepts `.[N]`, `.[A:B]`, `.[A:B:C]`, `.[A:]`, `.[:B]`,
//!    `.[::C]`. Bounds are decimal signed 64-bit integers.
//!
//! 2. **Schema check against runtime-types `Schema`.** The Haskell
//!    walker uses TypeU (compile-time type). The Rust walker uses
//!    `crate::schema::Schema` (runtime wire representation). A pattern
//!    that would type-check in Haskell on `[Int]` must also type-check
//!    here on the schema code `aj` (Array<Int>).
//!
//! The output of `parse_pattern` is a `Path`; the output of
//! `path_to_walker_input` is the placeholder-form path string and a
//! parallel `Vec<i64>` of literal values in DFS order (with sentinel
//! placeholders for absent slice bounds so the caller can drop them
//! into `IFileWalkArg[]` with a matching `has` byte).

use crate::error::MorlocError;
use crate::schema::{Schema, SerialType};

// ── AST ──────────────────────────────────────────────────────────────────

/// A parsed pattern-chain path.
///
/// Mirrors the Haskell `Path` type. The `Group` variant terminates the
/// walk and produces a Tuple whose element types come from the child
/// paths' walk results.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Path {
    Linear(Vec<LinearStep>),
    Group {
        prefix: Vec<LinearStep>,
        children: Vec<Path>,
    },
}

/// One linear step of a path.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LinearStep {
    /// `.foo`: record-field access.
    Field(String),
    /// `.0`, `.12`: tuple-position access.
    TupleIdx(u32),
    /// `.[]` (placeholder) or `.[N]` (literal). `arg = None` leaves
    /// the runtime IFileWalkArg to be supplied by the caller.
    BracketIndex { arg: Option<i64> },
    /// `.[:]`, `.[A:B]`, `.[A:B:C]`, `.[A:]`, `.[:B]`, `.[::C]`.
    /// Each bound is `Some(n)` when the user wrote a literal or
    /// `None` when they left it blank; the walker fills blanks with
    /// language defaults (start=0, stop=length, step=1 in Python-
    /// style).
    BracketSlice {
        start: Option<i64>,
        stop: Option<i64>,
        step: Option<i64>,
    },
}

// ── Parser ───────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    /// Empty input string.
    Empty,
    /// Unexpected end of input at position `pos`. `context` describes
    /// what the parser was expecting.
    UnexpectedEnd { pos: usize, context: &'static str },
    /// Unexpected character `ch` at position `pos`. `context` describes
    /// what the parser was expecting.
    Unexpected {
        pos: usize,
        ch: char,
        context: &'static str,
    },
    /// Integer literal was not a valid signed 64-bit decimal.
    BadInt { pos: usize, text: String },
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Empty => write!(f, "empty pattern"),
            ParseError::UnexpectedEnd { pos, context } => write!(
                f, "unexpected end of pattern at byte {} while parsing {}",
                pos, context
            ),
            ParseError::Unexpected { pos, ch, context } => write!(
                f, "unexpected {:?} at byte {} while parsing {}",
                ch, pos, context
            ),
            ParseError::BadInt { pos, text } => write!(
                f, "invalid integer {:?} at byte {}",
                text, pos
            ),
        }
    }
}

impl From<ParseError> for MorlocError {
    fn from(e: ParseError) -> Self {
        MorlocError::Packet(format!("pattern parse: {}", e))
    }
}

struct Parser<'a> {
    src: &'a [u8],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(src: &'a str) -> Self {
        Self { src: src.as_bytes(), pos: 0 }
    }

    fn peek(&self) -> Option<u8> {
        self.src.get(self.pos).copied()
    }

    fn eat(&mut self, byte: u8, context: &'static str) -> Result<(), ParseError> {
        match self.peek() {
            Some(b) if b == byte => {
                self.pos += 1;
                Ok(())
            }
            Some(b) => Err(ParseError::Unexpected {
                pos: self.pos,
                ch: b as char,
                context,
            }),
            None => Err(ParseError::UnexpectedEnd { pos: self.pos, context }),
        }
    }

    fn at_end(&self) -> bool {
        self.pos >= self.src.len()
    }
}

/// Parse a pattern-chain string into a `Path`.
///
/// Grammar (EBNF, with `[]` for optional):
///
/// ```text
/// path        = linear-step+
///             | linear-step* group-step
/// linear-step = "." field
///             | "." digits                       (StepTupleIdx)
///             | "." "[" [ int ] "]"              (BracketIndex)
///             | "." "[" slice "]"                (BracketSlice)
/// slice       = [int] ":" [int] [ ":" [int] ]
/// group-step  = "." "(" path (";" path)+ ")"
/// field       = (letter | "_") (letter | digit | "_")*
/// int         = ["-"] digit+
/// ```
pub fn parse_pattern(src: &str) -> Result<Path, ParseError> {
    if src.is_empty() {
        return Err(ParseError::Empty);
    }
    let mut p = Parser::new(src);
    let path = parse_path_inner(&mut p)?;
    if !p.at_end() {
        return Err(ParseError::Unexpected {
            pos: p.pos,
            ch: p.peek().unwrap_or(b'\0') as char,
            context: "trailing input after pattern",
        });
    }
    Ok(path)
}

fn parse_path_inner(p: &mut Parser<'_>) -> Result<Path, ParseError> {
    // Linear steps first. A ".(" begins a group; back off from the
    // linear step and switch to group parsing when we see it.
    let mut prefix = Vec::new();
    loop {
        // Every step begins with `.`.
        if p.peek() != Some(b'.') {
            break;
        }
        // Peek past the dot: `(` -> group; otherwise -> linear step.
        if p.src.get(p.pos + 1) == Some(&b'(') {
            break;
        }
        p.pos += 1; // consume `.`
        let step = parse_step_body(p)?;
        prefix.push(step);
    }

    // Optional terminating group.
    if p.peek() == Some(b'.') && p.src.get(p.pos + 1) == Some(&b'(') {
        p.pos += 1; // consume `.`
        let children = parse_group_body(p)?;
        Ok(Path::Group { prefix, children })
    } else if prefix.is_empty() {
        Err(ParseError::Unexpected {
            pos: p.pos,
            ch: p.peek().unwrap_or(b'\0') as char,
            context: "path (expected `.` for step or group)",
        })
    } else {
        Ok(Path::Linear(prefix))
    }
}

fn parse_step_body(p: &mut Parser<'_>) -> Result<LinearStep, ParseError> {
    match p.peek() {
        Some(b'[') => {
            p.pos += 1;
            parse_bracket_body(p)
        }
        Some(b'0'..=b'9') => {
            let (n, _) = parse_uint(p)?;
            Ok(LinearStep::TupleIdx(n as u32))
        }
        Some(b) if b == b'_' || (b as char).is_ascii_alphabetic() => {
            let name = parse_field_name(p);
            Ok(LinearStep::Field(name))
        }
        Some(b) => Err(ParseError::Unexpected {
            pos: p.pos,
            ch: b as char,
            context: "step body (expected `[`, digit, letter, or `_`)",
        }),
        None => Err(ParseError::UnexpectedEnd {
            pos: p.pos,
            context: "step body",
        }),
    }
}

fn parse_bracket_body(p: &mut Parser<'_>) -> Result<LinearStep, ParseError> {
    // Immediately after `[`. Two shapes:
    //   `]`               -> BracketIndex { arg: None }
    //   int `]`           -> BracketIndex { arg: Some(n) }
    //   [int] `:` [int] [ `:` [int] ] `]`   -> BracketSlice
    // Read up to `]` and parse.
    let start = p.pos;
    let mut end = start;
    while end < p.src.len() && p.src[end] != b']' {
        end += 1;
    }
    if end >= p.src.len() {
        return Err(ParseError::UnexpectedEnd {
            pos: p.pos,
            context: "bracket step (missing `]`)",
        });
    }
    let body = &p.src[start..end];
    p.pos = end + 1; // consume through `]`

    // Empty body: BracketIndex placeholder.
    if body.is_empty() {
        return Ok(LinearStep::BracketIndex { arg: None });
    }

    // Slice iff body contains `:` at least once.
    if body.contains(&b':') {
        let (start_v, stop_v, step_v) = parse_slice_body(body, start)?;
        Ok(LinearStep::BracketSlice {
            start: start_v,
            stop: stop_v,
            step: step_v,
        })
    } else {
        // Literal single-index: `[N]`.
        let n = parse_int_slice(body, start)?;
        Ok(LinearStep::BracketIndex { arg: Some(n) })
    }
}

fn parse_slice_body(
    body: &[u8],
    base_pos: usize,
) -> Result<(Option<i64>, Option<i64>, Option<i64>), ParseError> {
    // Split on `:` into at most 3 parts.
    let mut parts: Vec<&[u8]> = Vec::with_capacity(3);
    let mut cur = 0;
    for i in 0..body.len() {
        if body[i] == b':' {
            parts.push(&body[cur..i]);
            cur = i + 1;
        }
    }
    parts.push(&body[cur..]);
    if parts.len() > 3 {
        return Err(ParseError::Unexpected {
            pos: base_pos + body.iter().position(|&b| b == b':').unwrap(),
            ch: ':',
            context: "slice (too many `:`, expected at most 2)",
        });
    }
    let s = if parts[0].is_empty() {
        None
    } else {
        Some(parse_int_slice(parts[0], base_pos)?)
    };
    let e = if parts.len() < 2 || parts[1].is_empty() {
        None
    } else {
        Some(parse_int_slice(parts[1], base_pos)?)
    };
    let step = if parts.len() < 3 || parts[2].is_empty() {
        None
    } else {
        Some(parse_int_slice(parts[2], base_pos)?)
    };
    Ok((s, e, step))
}

fn parse_int_slice(bytes: &[u8], base_pos: usize) -> Result<i64, ParseError> {
    let s = std::str::from_utf8(bytes).map_err(|_| ParseError::BadInt {
        pos: base_pos,
        text: String::from_utf8_lossy(bytes).into_owned(),
    })?;
    s.parse::<i64>().map_err(|_| ParseError::BadInt {
        pos: base_pos,
        text: s.to_string(),
    })
}

fn parse_uint(p: &mut Parser<'_>) -> Result<(u64, usize), ParseError> {
    let start = p.pos;
    while let Some(b) = p.peek() {
        if (b as char).is_ascii_digit() {
            p.pos += 1;
        } else {
            break;
        }
    }
    let text = std::str::from_utf8(&p.src[start..p.pos]).unwrap();
    let n = text.parse::<u64>().map_err(|_| ParseError::BadInt {
        pos: start,
        text: text.to_string(),
    })?;
    Ok((n, start))
}

fn parse_field_name(p: &mut Parser<'_>) -> String {
    let start = p.pos;
    while let Some(b) = p.peek() {
        let c = b as char;
        if c.is_ascii_alphanumeric() || c == '_' {
            p.pos += 1;
        } else {
            break;
        }
    }
    std::str::from_utf8(&p.src[start..p.pos]).unwrap().to_string()
}

fn parse_group_body(p: &mut Parser<'_>) -> Result<Vec<Path>, ParseError> {
    p.eat(b'(', "group opening `(`")?;
    let mut children = Vec::new();
    children.push(parse_path_inner(p)?);
    // Accept both `,` (morloc source syntax, e.g. `.(.a,.b)` in a
    // `.loc` file) and `;` (the runtime walker's placeholder syntax,
    // e.g. the encoded form in `walkStepsToPath`). Same semantic;
    // users shouldn't have to remember which context they're in.
    while matches!(p.peek(), Some(b';') | Some(b',')) {
        p.pos += 1;
        children.push(parse_path_inner(p)?);
    }
    p.eat(b')', "group closing `)`")?;
    Ok(children)
}

// ── Schema-check walker ─────────────────────────────────────────────────

/// Errors from walking a `Path` against a `Schema`.
#[derive(Debug, Clone)]
pub enum WalkError {
    /// `.foo` on a record that has no `foo` field.
    FieldNotFound {
        field: String,
        schema_type: SerialType,
    },
    /// `.N` on a tuple whose arity is <= N.
    TupleIdxOutOfRange {
        idx: u32,
        arity: usize,
    },
    /// `.foo` on a non-record type.
    ExpectedRecord { got: SerialType },
    /// `.N` on a non-tuple type.
    ExpectedTuple { got: SerialType },
    /// `.[]` or `.[N]` on a type not known to be indexable.
    ExpectedIndexable { got: SerialType },
    /// `.[:]` or `.[A:B]` on a type not known to be sliceable.
    ExpectedSliceable { got: SerialType },
    /// The Rust walker doesn't recognize this type as Indexable/
    /// Sliceable even though the compile-time (Haskell) walker may
    /// accept it. Directs the user to open an issue / extend the
    /// Rust walker's instance set.
    Unhandled {
        got: SerialType,
        reason: &'static str,
    },
}

impl std::fmt::Display for WalkError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WalkError::FieldNotFound { field, schema_type } => write!(
                f, "pattern does not match schema: field {:?} not found on {:?}",
                field, schema_type
            ),
            WalkError::TupleIdxOutOfRange { idx, arity } => write!(
                f, "pattern does not match schema: tuple index {} out of range (arity {})",
                idx, arity
            ),
            WalkError::ExpectedRecord { got } => write!(
                f, "pattern does not match schema: expected record, got {:?}",
                got
            ),
            WalkError::ExpectedTuple { got } => write!(
                f, "pattern does not match schema: expected tuple, got {:?}",
                got
            ),
            WalkError::ExpectedIndexable { got } => write!(
                f, "pattern does not match schema: expected indexable, got {:?}",
                got
            ),
            WalkError::ExpectedSliceable { got } => write!(
                f, "pattern does not match schema: expected sliceable, got {:?}",
                got
            ),
            WalkError::Unhandled { got, reason } => write!(
                f,
                "pattern hit a type ({:?}) that Rust's runtime pattern checker \
                 does not yet recognise as {}. If this pattern is accepted by \
                 the morloc compiler, please open an issue.",
                got, reason
            ),
        }
    }
}

/// Compute the extracted schema given a receiver schema and a parsed
/// pattern. On success, returns the result schema (what the walker
/// would emit at runtime).
pub fn check_pattern_against_schema(
    path: &Path,
    input: &Schema,
) -> Result<Schema, WalkError> {
    match path {
        Path::Linear(steps) => walk_steps(input, steps),
        Path::Group { prefix, children } => {
            let mid = walk_steps(input, prefix)?;
            // Post-slice group broadcast: `.[:].(.a;.b)` on `[T]` is
            // `[T -> (T.a, T.b)]` per IntrMap desugar. When the prefix
            // ends with a slice and the mid-type is a list, walk each
            // child against the ELEMENT type and wrap the resulting
            // tuple in a list.
            let prefix_ends_with_slice = prefix
                .last()
                .map(|s| matches!(s, LinearStep::BracketSlice { .. }))
                .unwrap_or(false);
            if prefix_ends_with_slice
                && mid.serial_type == SerialType::Array
                && !mid.parameters.is_empty()
            {
                let elem = mid.parameters[0].clone();
                let child_schemas: Result<Vec<Schema>, _> = children
                    .iter()
                    .map(|c| check_pattern_against_schema(c, &elem))
                    .collect();
                let tuple = make_tuple(child_schemas?);
                return Ok(array_of(tuple));
            }
            let child_schemas: Result<Vec<Schema>, _> = children
                .iter()
                .map(|c| check_pattern_against_schema(c, &mid))
                .collect();
            let child_schemas = child_schemas?;
            Ok(make_tuple(child_schemas))
        }
    }
}

fn walk_steps(input: &Schema, steps: &[LinearStep]) -> Result<Schema, WalkError> {
    let mut cur = input.clone();
    let mut i = 0;
    while i < steps.len() {
        let s = &steps[i];
        cur = walk_step(&cur, s)?;
        // Post-slice broadcast: `.[:]` with a non-empty tail walks the
        // tail on the element type and wraps the result in a list,
        // mirroring the IntrMap desugar.
        if matches!(s, LinearStep::BracketSlice { .. })
            && i + 1 < steps.len()
            && cur.serial_type == SerialType::Array
            && !cur.parameters.is_empty()
        {
            let elem = cur.parameters[0].clone();
            let inner = walk_steps(&elem, &steps[i + 1..])?;
            return Ok(array_of(inner));
        }
        i += 1;
    }
    Ok(cur)
}

fn walk_step(schema: &Schema, step: &LinearStep) -> Result<Schema, WalkError> {
    match step {
        LinearStep::Field(k) => match schema.serial_type {
            SerialType::Map => {
                let idx = schema.keys.iter().position(|kk| kk == k);
                match idx {
                    Some(i) if i < schema.parameters.len() => {
                        Ok(schema.parameters[i].clone())
                    }
                    _ => Err(WalkError::FieldNotFound {
                        field: k.clone(),
                        schema_type: schema.serial_type,
                    }),
                }
            }
            other => Err(WalkError::ExpectedRecord { got: other }),
        },
        LinearStep::TupleIdx(i) => match schema.serial_type {
            SerialType::Tuple => {
                if (*i as usize) < schema.parameters.len() {
                    Ok(schema.parameters[*i as usize].clone())
                } else {
                    Err(WalkError::TupleIdxOutOfRange {
                        idx: *i,
                        arity: schema.parameters.len(),
                    })
                }
            }
            other => Err(WalkError::ExpectedTuple { got: other }),
        },
        LinearStep::BracketIndex { .. } => match schema.serial_type {
            SerialType::Array => {
                if schema.parameters.is_empty() {
                    Err(WalkError::Unhandled {
                        got: schema.serial_type,
                        reason: "Array with no element schema",
                    })
                } else {
                    Ok(schema.parameters[0].clone())
                }
            }
            SerialType::String => Ok(schema.clone()),
            other => Err(WalkError::ExpectedIndexable { got: other }),
        },
        LinearStep::BracketSlice { .. } => match schema.serial_type {
            SerialType::Array | SerialType::String => Ok(schema.clone()),
            other => Err(WalkError::ExpectedSliceable { got: other }),
        },
    }
}

/// Wrap `elem` in an `Array<T>` schema. Used by the walker's
/// broadcast rule when a `.field`/`.N` step is applied to a list.
fn array_of(elem: Schema) -> Schema {
    Schema {
        serial_type: SerialType::Array,
        size: 1,
        width: std::mem::size_of::<crate::shm_types::Array>(),
        offsets: vec![0],
        hint: None,
        parameters: vec![elem],
        keys: Vec::new(),
        name: None,
    }
}

fn make_tuple(children: Vec<Schema>) -> Schema {
    // Single-child group is idempotent (unwrap the child).
    if children.len() == 1 {
        return children.into_iter().next().unwrap();
    }
    let width: usize = children.iter().map(|c| c.width).sum();
    let mut offsets = Vec::with_capacity(children.len());
    let mut off = 0usize;
    for c in &children {
        offsets.push(off);
        off += c.width;
    }
    Schema {
        serial_type: SerialType::Tuple,
        size: children.len(),
        width,
        offsets,
        hint: None,
        parameters: children,
        keys: Vec::new(),
        name: None,
    }
}

// ── Runtime-walker encoding ─────────────────────────────────────────────

/// One encoded literal for the runtime walker's `IFileWalkArg` array.
/// `has = 1` means the caller supplied a literal; `has = 0` means the
/// walker should apply its default (0 for bracket-index, start=0 /
/// stop=length / step=1 for slice bounds).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EncodedArg {
    pub has: u8,
    pub value: i64,
}

/// Convert a parsed `Path` into the runtime walker's placeholder-form
/// path string plus the parallel `EncodedArg` array. DFS order matches
/// the runtime walker's expectation: each `BracketIndex` consumes one
/// arg, each `BracketSlice` consumes three (start, stop, step),
/// recursing into group children after any linear prefix.
pub fn path_to_walker_input(path: &Path) -> (String, Vec<EncodedArg>) {
    let mut out_path = String::new();
    let mut args = Vec::new();
    encode_path(path, &mut out_path, &mut args);
    (out_path, args)
}

fn encode_path(path: &Path, out: &mut String, args: &mut Vec<EncodedArg>) {
    match path {
        Path::Linear(steps) => encode_steps(steps, out, args),
        Path::Group { prefix, children } => {
            encode_steps(prefix, out, args);
            out.push('.');
            out.push('(');
            for (i, c) in children.iter().enumerate() {
                if i > 0 {
                    out.push(';');
                }
                encode_path(c, out, args);
            }
            out.push(')');
        }
    }
}

fn encode_steps(steps: &[LinearStep], out: &mut String, args: &mut Vec<EncodedArg>) {
    for s in steps {
        match s {
            LinearStep::Field(name) => {
                out.push('.');
                out.push_str(name);
            }
            LinearStep::TupleIdx(i) => {
                out.push('.');
                out.push_str(&i.to_string());
            }
            LinearStep::BracketIndex { arg } => {
                out.push_str(".[]");
                args.push(encode_opt(*arg));
            }
            LinearStep::BracketSlice { start, stop, step } => {
                out.push_str(".[:]");
                args.push(encode_opt(*start));
                args.push(encode_opt(*stop));
                args.push(encode_opt(*step));
            }
        }
    }
}

fn encode_opt(v: Option<i64>) -> EncodedArg {
    match v {
        Some(n) => EncodedArg { has: 1, value: n },
        None => EncodedArg { has: 0, value: 0 },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ── Parser ────────────────────────────────────────────────────────

    #[test]
    fn parse_simple_field() {
        let p = parse_pattern(".foo").unwrap();
        assert_eq!(p, Path::Linear(vec![LinearStep::Field("foo".into())]));
    }

    #[test]
    fn parse_tuple_idx() {
        let p = parse_pattern(".0.12").unwrap();
        assert_eq!(
            p,
            Path::Linear(vec![
                LinearStep::TupleIdx(0),
                LinearStep::TupleIdx(12),
            ])
        );
    }

    #[test]
    fn parse_placeholder_bracket_index() {
        let p = parse_pattern(".[]").unwrap();
        assert_eq!(
            p,
            Path::Linear(vec![LinearStep::BracketIndex { arg: None }])
        );
    }

    #[test]
    fn parse_literal_bracket_index() {
        let p = parse_pattern(".[42]").unwrap();
        assert_eq!(
            p,
            Path::Linear(vec![LinearStep::BracketIndex { arg: Some(42) }])
        );
    }

    #[test]
    fn parse_negative_bracket_index() {
        let p = parse_pattern(".[-3]").unwrap();
        assert_eq!(
            p,
            Path::Linear(vec![LinearStep::BracketIndex { arg: Some(-3) }])
        );
    }

    #[test]
    fn parse_slice_all_bounds() {
        let p = parse_pattern(".[10:20:2]").unwrap();
        assert_eq!(
            p,
            Path::Linear(vec![LinearStep::BracketSlice {
                start: Some(10),
                stop: Some(20),
                step: Some(2),
            }])
        );
    }

    #[test]
    fn parse_slice_open_bounds() {
        let p = parse_pattern(".[:10]").unwrap();
        assert_eq!(
            p,
            Path::Linear(vec![LinearStep::BracketSlice {
                start: None,
                stop: Some(10),
                step: None,
            }])
        );
        let p = parse_pattern(".[10:]").unwrap();
        assert_eq!(
            p,
            Path::Linear(vec![LinearStep::BracketSlice {
                start: Some(10),
                stop: None,
                step: None,
            }])
        );
        let p = parse_pattern(".[::5]").unwrap();
        assert_eq!(
            p,
            Path::Linear(vec![LinearStep::BracketSlice {
                start: None,
                stop: None,
                step: Some(5),
            }])
        );
        let p = parse_pattern(".[:]").unwrap();
        assert_eq!(
            p,
            Path::Linear(vec![LinearStep::BracketSlice {
                start: None,
                stop: None,
                step: None,
            }])
        );
    }

    #[test]
    fn parse_chain_field_bracket() {
        let p = parse_pattern(".foo.[101:200]").unwrap();
        assert_eq!(
            p,
            Path::Linear(vec![
                LinearStep::Field("foo".into()),
                LinearStep::BracketSlice {
                    start: Some(101),
                    stop: Some(200),
                    step: None,
                },
            ])
        );
    }

    #[test]
    fn parse_group() {
        let p = parse_pattern(".(.a;.b)").unwrap();
        assert_eq!(
            p,
            Path::Group {
                prefix: vec![],
                children: vec![
                    Path::Linear(vec![LinearStep::Field("a".into())]),
                    Path::Linear(vec![LinearStep::Field("b".into())]),
                ],
            }
        );
    }

    #[test]
    fn parse_group_accepts_morloc_comma_separator() {
        // Users typing patterns from a .loc file expect `,` between
        // group children (e.g. `.[0:10].(.0,.2)`). The parser must
        // accept both `,` and `;` so the surface syntax matches the
        // morloc source pattern grammar.
        let p_comma = parse_pattern(".(.a,.b)").unwrap();
        let p_semi  = parse_pattern(".(.a;.b)").unwrap();
        assert_eq!(p_comma, p_semi);
    }

    #[test]
    fn parse_chain_with_group_terminator() {
        let p = parse_pattern(".[0:5].(.name;.age)").unwrap();
        assert_eq!(
            p,
            Path::Group {
                prefix: vec![LinearStep::BracketSlice {
                    start: Some(0),
                    stop: Some(5),
                    step: None,
                }],
                children: vec![
                    Path::Linear(vec![LinearStep::Field("name".into())]),
                    Path::Linear(vec![LinearStep::Field("age".into())]),
                ],
            }
        );
    }

    #[test]
    fn parse_error_empty() {
        assert_eq!(parse_pattern(""), Err(ParseError::Empty));
    }

    #[test]
    fn parse_error_missing_close_bracket() {
        let e = parse_pattern(".[10").unwrap_err();
        assert!(matches!(e, ParseError::UnexpectedEnd { .. }));
    }

    #[test]
    fn parse_error_trailing_garbage() {
        let e = parse_pattern(".foo!").unwrap_err();
        assert!(matches!(e, ParseError::Unexpected { .. }));
    }

    #[test]
    fn parse_error_no_leading_dot() {
        let e = parse_pattern("foo").unwrap_err();
        assert!(matches!(e, ParseError::Unexpected { .. }));
    }

    // ── Encoding ──────────────────────────────────────────────────────

    #[test]
    fn encode_placeholder_index() {
        let p = parse_pattern(".[]").unwrap();
        let (path, args) = path_to_walker_input(&p);
        assert_eq!(path, ".[]");
        assert_eq!(args, vec![EncodedArg { has: 0, value: 0 }]);
    }

    #[test]
    fn encode_literal_slice() {
        let p = parse_pattern(".[10:20]").unwrap();
        let (path, args) = path_to_walker_input(&p);
        assert_eq!(path, ".[:]");
        assert_eq!(
            args,
            vec![
                EncodedArg { has: 1, value: 10 },
                EncodedArg { has: 1, value: 20 },
                EncodedArg { has: 0, value: 0 },
            ]
        );
    }

    #[test]
    fn encode_group_walk_dfs() {
        let p = parse_pattern(".[0:5].(.a;.b)").unwrap();
        let (path, args) = path_to_walker_input(&p);
        assert_eq!(path, ".[:].(.a;.b)");
        // Only the outer slice contributes args; the group children
        // are field-only.
        assert_eq!(
            args,
            vec![
                EncodedArg { has: 1, value: 0 },
                EncodedArg { has: 1, value: 5 },
                EncodedArg { has: 0, value: 0 },
            ]
        );
    }

    // ── Schema check ──────────────────────────────────────────────────

    fn scalar(t: SerialType) -> Schema {
        Schema::primitive(t)
    }

    fn array_of(elem: Schema) -> Schema {
        Schema {
            serial_type: SerialType::Array,
            size: 1,
            width: 16,
            offsets: vec![],
            hint: None,
            parameters: vec![elem],
            keys: vec![],
            name: None,
        }
    }

    fn tuple_of(children: Vec<Schema>) -> Schema {
        let width: usize = children.iter().map(|c| c.width).sum();
        let mut offsets = Vec::with_capacity(children.len());
        let mut off = 0usize;
        for c in &children {
            offsets.push(off);
            off += c.width;
        }
        Schema {
            serial_type: SerialType::Tuple,
            size: children.len(),
            width,
            offsets,
            hint: None,
            parameters: children,
            keys: vec![],
            name: None,
        }
    }

    fn record_of(fields: Vec<(&str, Schema)>) -> Schema {
        let keys: Vec<String> = fields.iter().map(|(k, _)| k.to_string()).collect();
        let params: Vec<Schema> = fields.into_iter().map(|(_, v)| v).collect();
        let width: usize = params.iter().map(|c| c.width).sum();
        let mut offsets = Vec::with_capacity(params.len());
        let mut off = 0usize;
        for c in &params {
            offsets.push(off);
            off += c.width;
        }
        Schema {
            serial_type: SerialType::Map,
            size: params.len(),
            width,
            offsets,
            hint: None,
            parameters: params,
            keys,
            name: None,
        }
    }

    #[test]
    fn check_bracket_on_array() {
        let s = array_of(scalar(SerialType::Sint64));
        let p = parse_pattern(".[10:20]").unwrap();
        let r = check_pattern_against_schema(&p, &s).unwrap();
        // Slice preserves the receiver type.
        assert_eq!(r.serial_type, SerialType::Array);
    }

    #[test]
    fn check_bracket_index_returns_element() {
        let s = array_of(scalar(SerialType::Sint64));
        let p = parse_pattern(".[5]").unwrap();
        let r = check_pattern_against_schema(&p, &s).unwrap();
        assert_eq!(r.serial_type, SerialType::Sint64);
    }

    #[test]
    fn check_field_on_record() {
        let s = record_of(vec![
            ("name", scalar(SerialType::String)),
            ("age", scalar(SerialType::Sint64)),
        ]);
        let p = parse_pattern(".name").unwrap();
        let r = check_pattern_against_schema(&p, &s).unwrap();
        assert_eq!(r.serial_type, SerialType::String);
    }

    #[test]
    fn check_field_not_found() {
        let s = record_of(vec![("name", scalar(SerialType::String))]);
        let p = parse_pattern(".age").unwrap();
        let e = check_pattern_against_schema(&p, &s).unwrap_err();
        assert!(matches!(e, WalkError::FieldNotFound { .. }));
    }

    #[test]
    fn check_group_returns_tuple() {
        let s = array_of(record_of(vec![
            ("name", scalar(SerialType::String)),
            ("age", scalar(SerialType::Sint64)),
        ]));
        // Bracket-index reduces the array to one record; group then
        // projects two fields, producing a Tuple(String, Sint64).
        let p = parse_pattern(".[0].(.name;.age)").unwrap();
        let r = check_pattern_against_schema(&p, &s).unwrap();
        assert_eq!(r.serial_type, SerialType::Tuple);
        assert_eq!(r.parameters.len(), 2);
        assert_eq!(r.parameters[0].serial_type, SerialType::String);
        assert_eq!(r.parameters[1].serial_type, SerialType::Sint64);
    }

    #[test]
    fn check_bare_field_on_list_errors() {
        // Bare `.field` on `[Record]` is NOT broadcast: the runtime
        // walker's `shared_ifile_general` doesn't handle this case
        // (only slice-with-tail broadcasts). Rejecting at type-check
        // gives a clean error instead of a runtime memory blow-up.
        let s = array_of(record_of(vec![
            ("name", scalar(SerialType::String)),
        ]));
        let p = parse_pattern(".name").unwrap();
        let e = check_pattern_against_schema(&p, &s).unwrap_err();
        assert!(matches!(e, WalkError::ExpectedRecord { .. }));
    }

    #[test]
    fn check_slice_then_tuple_idx_broadcasts() {
        // The exact motivating case from the user report:
        // `at3sau1au1` (Array of Tuple3<String, Bytes, Bytes>), pattern
        // `.[0:2].0` should type as `[String]` -- slice preserves the
        // list, then `.0` broadcasts over each tuple.
        let bytes = array_of(scalar(SerialType::Uint8));
        let tup = tuple_of(vec![
            scalar(SerialType::String),
            bytes.clone(),
            bytes,
        ]);
        let s = array_of(tup);
        let p = parse_pattern(".[0:2].0").unwrap();
        let r = check_pattern_against_schema(&p, &s).unwrap();
        assert_eq!(r.serial_type, SerialType::Array);
        assert_eq!(r.parameters.len(), 1);
        assert_eq!(r.parameters[0].serial_type, SerialType::String);
    }

    #[test]
    fn check_slice_then_field_chain_broadcasts() {
        // `.[i:j].outer.inner` on `[{outer:{inner:Int}}]` broadcasts
        // the whole `.outer.inner` chain over the slice: result is
        // `[Int]`.
        let inner_rec = record_of(vec![("inner", scalar(SerialType::Sint64))]);
        let outer_rec = record_of(vec![("outer", inner_rec)]);
        let s = array_of(outer_rec);
        let p = parse_pattern(".[0:5].outer.inner").unwrap();
        let r = check_pattern_against_schema(&p, &s).unwrap();
        assert_eq!(r.serial_type, SerialType::Array);
        assert_eq!(r.parameters.len(), 1);
        assert_eq!(r.parameters[0].serial_type, SerialType::Sint64);
    }

    #[test]
    fn check_field_on_non_record_scalar_still_errors() {
        // Broadcast only fires when the receiver is a list. On a bare
        // scalar `.foo` still errors -- confirming we don't silently
        // over-accept.
        let s = scalar(SerialType::Sint64);
        let p = parse_pattern(".foo").unwrap();
        let e = check_pattern_against_schema(&p, &s).unwrap_err();
        assert!(matches!(e, WalkError::ExpectedRecord { .. }));
    }

    #[test]
    fn check_bracket_on_scalar_errors() {
        let s = scalar(SerialType::Sint64);
        let p = parse_pattern(".[0]").unwrap();
        let e = check_pattern_against_schema(&p, &s).unwrap_err();
        assert!(matches!(e, WalkError::ExpectedIndexable { .. }));
    }

    #[test]
    fn check_tuple_idx_on_tuple() {
        let s = tuple_of(vec![
            scalar(SerialType::Sint64),
            scalar(SerialType::String),
        ]);
        let p = parse_pattern(".1").unwrap();
        let r = check_pattern_against_schema(&p, &s).unwrap();
        assert_eq!(r.serial_type, SerialType::String);
    }

    #[test]
    fn check_tuple_idx_out_of_range() {
        let s = tuple_of(vec![scalar(SerialType::Sint64)]);
        let p = parse_pattern(".5").unwrap();
        let e = check_pattern_against_schema(&p, &s).unwrap_err();
        assert!(matches!(e, WalkError::TupleIdxOutOfRange { .. }));
    }
}
