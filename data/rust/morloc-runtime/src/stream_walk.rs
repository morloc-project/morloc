//! The walk path language of `@walk` on an `IFile`: parsing a path and
//! following it through a materialized sub-packet. Pure: no registry, lock
//! or process state.

use morloc_runtime_types::schema::{Schema, SerialType};
use morloc_runtime_types::shm_types::{self as shm_types_crate};
use morloc_runtime_types::{slice, width};

use crate::error::MorlocError;
use crate::shm::{self, AbsPtr};
use crate::stream::*;
use crate::voidstar;

/// Parse a `.<step>.<step>...` suffix consisting purely of Field/Key
/// steps. Returns `None` if any bracket, group, or other non-field
/// step is present -- the caller falls back to the general walker
/// (which handles slice+group via broadcast_slice_tail). Used to
/// detect chain-fusable tails after a root-level `.[:]`.
pub(crate) fn parse_field_only_tail(suffix: &str) -> Result<Option<Vec<WalkStep>>, MorlocError> {
    let bytes = suffix.as_bytes();
    let mut out = Vec::new();
    let mut pos = 0;
    while pos < bytes.len() {
        if bytes[pos] != b'.' {
            return Err(MorlocError::Other(format!(
                "tail walk: expected '.' at byte {}, found {:?}",
                pos, bytes[pos] as char
            )));
        }
        pos += 1;
        if pos >= bytes.len() {
            return Err(MorlocError::Other(
                "tail walk: trailing '.' with no step".into(),
            ));
        }
        match bytes[pos] {
            b'0'..=b'9' => {
                let start = pos;
                while pos < bytes.len() && bytes[pos].is_ascii_digit() {
                    pos += 1;
                }
                let n: usize = std::str::from_utf8(&bytes[start..pos])
                    .unwrap()
                    .parse()
                    .map_err(|e: std::num::ParseIntError| MorlocError::Other(format!(
                        "tail walk: bad field index '{}': {}",
                        std::str::from_utf8(&bytes[start..pos]).unwrap_or("?"), e
                    )))?;
                out.push(WalkStep::Field(FieldStep::Index(n)));
            }
            b'_' | b'A'..=b'Z' | b'a'..=b'z' => {
                let start = pos;
                while pos < bytes.len()
                    && (bytes[pos] == b'_'
                        || bytes[pos].is_ascii_alphanumeric())
                {
                    pos += 1;
                }
                let name = std::str::from_utf8(&bytes[start..pos])
                    .map_err(|_| MorlocError::Other(
                        "tail walk: non-UTF-8 key name".into()
                    ))?
                    .to_string();
                out.push(WalkStep::Field(FieldStep::Key(name)));
            }
            _ => return Ok(None),
        }
    }
    Ok(Some(out))
}

/// Walk a sequence of Field/Key steps statically on the schema and
/// return the cumulative byte offset and the schema at the end of the
/// chain. Used by chain-fused bracket-slice to compute, once per
/// slice, where inside each record the projected sub-field lives.
///
/// Rejects any non-Field tail step. BracketIndex/BracketSlice in a
/// tail would need runtime args + per-element walking; we route those
/// through the general walker (`ifile_general`) instead.
pub(crate) fn navigate_static_field_offset(
    start: &Schema,
    steps: &[WalkStep],
) -> Result<(usize, Schema), MorlocError> {
    let mut off = 0usize;
    let mut cur = start.clone();
    for (i, step) in steps.iter().enumerate() {
        let f = match step {
            WalkStep::Field(f) => f,
            other => return Err(MorlocError::Other(format!(
                "navigate_static_field_offset: only Field/Key tail steps are supported, \
                 got {:?} at step {}", other, i
            ))),
        };
        let field_idx = match f {
            FieldStep::Index(i) => *i,
            FieldStep::Key(k) => cur.keys.iter().position(|sk| sk == k).ok_or_else(|| {
                MorlocError::Other(format!(
                    "static-field walk: key '{}' not found at step {} (keys = {:?})",
                    k, i, cur.keys
                ))
            })?,
        };
        if field_idx >= cur.parameters.len() {
            return Err(MorlocError::Other(format!(
                "static-field walk: field index {} out of range at step {} (schema has {} params)",
                field_idx, i, cur.parameters.len()
            )));
        }
        if field_idx >= cur.offsets.len() {
            return Err(MorlocError::Other(format!(
                "static-field walk: schema offsets[{}] missing at step {}",
                field_idx, i
            )));
        }
        off += cur.offsets[field_idx];
        cur = cur.parameters[field_idx].clone();
    }
    Ok((off, cur))
}

#[derive(Debug, Clone)]
pub(crate) enum FieldStep {
    Index(usize),
    Key(String),
}

#[derive(Debug)]
pub(crate) enum WalkStep {
    Field(FieldStep),
    /// Bracket-index step: consumes 1 runtime arg from the DFS-ordered
    /// args list. Result schema is the element type of the surrounding
    /// Array. Legal inside group children and as a leaf in any chain.
    BracketIndex,
    /// Bracket-slice step: consumes 3 runtime args (start, stop, step).
    /// Result schema is the surrounding Array type (length may change).
    /// Bracket-slice steps are TERMINAL within a chain: the path
    /// "...[:]X" with anything after is rejected.
    BracketSlice,
    /// Multi-field group: each child is its own sub-walk that
    /// operates at the same position. Result is a fresh tuple of the
    /// sibling values, in source order. Nested groups are supported
    /// (child chains may themselves contain Group steps). Empty
    /// groups (`.()`) materialise a Nil/unit value at this position.
    Group(Vec<Vec<WalkStep>>),
}

/// Parse a walk path into a chain of `WalkStep`s.
///
/// Grammar:
/// ```text
///   path        ::= step path | ε
///   step        ::= "." segment
///   segment     ::= int | name | "[]" | "[:]" | group
///   group       ::= "(" path { ";" path } ")"
/// ```
///
/// Invariants enforced here:
///
/// * A group is terminal in its parent chain -- nothing may follow.
///   `.(.x;.y).0` is rejected.
/// * `BracketSlice` is terminal in its chain (it returns a list; a
///   structural step after a list of values is morloc's `IntrMap`
///   territory, not a single walker call).
/// * A group with exactly one child is rejected -- a single-sibling
///   group is the non-grouped chain and the encoder normalises it.
/// * Empty groups `.()` are permitted and materialise a Nil/unit value
///   (an empty tuple) at the current position.
/// * Empty *child chains* (e.g. `.(.x;)`) are still rejected -- they
///   are unambiguously malformed and not the same as an empty group.
pub(crate) fn parse_walk_path(path: &str) -> Result<Vec<WalkStep>, MorlocError> {
    let bytes = path.as_bytes();
    let (steps, end) = parse_walk_seq(bytes, 0, /*depth=*/0)?;
    if end != bytes.len() {
        return Err(MorlocError::Other(format!(
            "walk path '{}' has trailing input at byte {}", path, end
        )));
    }
    if steps.is_empty() {
        return Err(MorlocError::Other(format!(
            "walk path '{}' contains no steps", path
        )));
    }
    Ok(steps)
}

/// Parse zero or more walk steps starting at byte `pos`. Stops at
/// end-of-input or at the first ';' / ')' belonging to an enclosing
/// group. Enforces "Group is terminal" and "BracketSlice is terminal"
/// within the produced chain.
pub(crate) fn parse_walk_seq(
    bytes: &[u8],
    mut pos: usize,
    depth: usize,
) -> Result<(Vec<WalkStep>, usize), MorlocError> {
    // Hard depth cap: protects against pathological deeply-nested
    // paths constructed by a misbehaving codegen.
    const MAX_DEPTH: usize = 64;
    if depth > MAX_DEPTH {
        return Err(MorlocError::Other(
            "walk path too deeply nested".into(),
        ));
    }
    let mut out: Vec<WalkStep> = Vec::new();
    while pos < bytes.len() && bytes[pos] != b';' && bytes[pos] != b')' {
        // Terminal-step enforcement: nothing may follow a Group in
        // the same chain (a group already materialises a fresh value
        // that isn't necessarily well-defined for further walking).
        // `BracketSlice` is NOT terminal: `.[:]<tail>` broadcasts
        // <tail> over each element of the slice, matching the
        // compiler's IntrMap desugar. `BracketIndex` is also not
        // terminal.
        if let Some(WalkStep::Group(_)) = out.last() {
            return Err(MorlocError::Other(
                "walk path: groups are terminal -- nothing may follow `.(.x;.y)`".into()
            ));
        }
        if bytes[pos] != b'.' {
            return Err(MorlocError::Other(format!(
                "walk path: expected '.' at byte {}, found {:?}",
                pos, bytes[pos] as char
            )));
        }
        pos += 1;
        if pos >= bytes.len() {
            return Err(MorlocError::Other(
                "walk path: trailing '.' with no step".into(),
            ));
        }
        match bytes[pos] {
            b'(' => {
                pos += 1;
                let mut chains: Vec<Vec<WalkStep>> = Vec::new();
                // Special-case the empty group ".()": consume the
                // closing ')' immediately and emit Group with zero
                // children. This materialises as a Nil/unit value.
                if pos < bytes.len() && bytes[pos] == b')' {
                    pos += 1;
                    out.push(WalkStep::Group(chains));
                    continue;
                }
                loop {
                    let (chain, next) = parse_walk_seq(bytes, pos, depth + 1)?;
                    if chain.is_empty() {
                        return Err(MorlocError::Other(
                            "walk path: empty child chain in group (e.g. '.(.x;)'); for an \
                             empty tuple use '.()' instead".into(),
                        ));
                    }
                    chains.push(chain);
                    pos = next;
                    if pos >= bytes.len() {
                        return Err(MorlocError::Other(
                            "walk path: unclosed group".into(),
                        ));
                    }
                    match bytes[pos] {
                        b';' => { pos += 1; continue; }
                        b')' => { pos += 1; break; }
                        c => return Err(MorlocError::Other(format!(
                            "walk path: expected ';' or ')' inside group, got {:?}",
                            c as char
                        ))),
                    }
                }
                if chains.len() == 1 {
                    // Single-child groups are illegal: morloc has no
                    // 1-tuple, and the encoder collapses single-child
                    // groups into the non-grouped chain before
                    // emission. Anything reaching us here is a
                    // malformed encoding.
                    return Err(MorlocError::Other(
                        "walk path: single-child groups are illegal -- there is no 1-tuple; \
                         the encoder should have normalised `.(.x)` to `.x`".into(),
                    ));
                }
                out.push(WalkStep::Group(chains));
            }
            b'[' => {
                pos += 1;
                // Distinguish "[]" (bracket-index) from "[:]" (bracket-slice).
                match bytes.get(pos).copied() {
                    Some(b']') => {
                        pos += 1;
                        out.push(WalkStep::BracketIndex);
                    }
                    Some(b':') => {
                        pos += 1;
                        match bytes.get(pos).copied() {
                            Some(b']') => {
                                pos += 1;
                                out.push(WalkStep::BracketSlice);
                            }
                            other => return Err(MorlocError::Other(format!(
                                "walk path: expected ']' after '[:', got {:?}",
                                other.map(|c| c as char)
                            ))),
                        }
                    }
                    other => return Err(MorlocError::Other(format!(
                        "walk path: expected ']' or ':' after '[', got {:?}",
                        other.map(|c| c as char)
                    ))),
                }
            }
            _ => {
                // Field step: read a maximal run of [A-Za-z0-9_].
                let start = pos;
                while pos < bytes.len() {
                    let c = bytes[pos];
                    if c == b'.' || c == b';' || c == b')' { break; }
                    if !(c.is_ascii_alphanumeric() || c == b'_') {
                        return Err(MorlocError::Other(format!(
                            "walk path: illegal character {:?} at byte {}",
                            c as char, pos
                        )));
                    }
                    pos += 1;
                }
                if pos == start {
                    return Err(MorlocError::Other(format!(
                        "walk path: empty step at byte {}", start
                    )));
                }
                let seg = std::str::from_utf8(&bytes[start..pos])
                    .map_err(|e| MorlocError::Other(format!(
                        "walk path: non-UTF8 segment ({})", e
                    )))?;
                if let Ok(i) = seg.parse::<usize>() {
                    out.push(WalkStep::Field(FieldStep::Index(i)));
                } else {
                    out.push(WalkStep::Field(FieldStep::Key(seg.to_string())));
                }
            }
        }
    }
    Ok((out, pos))
}

/// Stateful cursor over the DFS-ordered runtime args list. Each
/// bracket step consumes 1 (index) or 3 (slice) args from the front;
/// every other step consumes none.
pub(crate) struct ArgsCursor<'a> {
    args: &'a [crate::intrinsics::IFileWalkArg],
    pos: usize,
}

impl<'a> ArgsCursor<'a> {
    fn new(args: &'a [crate::intrinsics::IFileWalkArg]) -> Self {
        Self { args, pos: 0 }
    }
    fn next_one(&mut self) -> Result<&'a crate::intrinsics::IFileWalkArg, MorlocError> {
        if self.pos >= self.args.len() {
            return Err(MorlocError::Other(
                "walk: ran out of runtime args (bracket step expected an index/bound)".into(),
            ));
        }
        let r = &self.args[self.pos];
        self.pos += 1;
        Ok(r)
    }
    fn next_three(&mut self) -> Result<
        (Option<i64>, Option<i64>, Option<i64>),
        MorlocError,
    > {
        let a = *self.next_one()?;
        let b = *self.next_one()?;
        let c = *self.next_one()?;
        let opt = |x: &crate::intrinsics::IFileWalkArg| {
            if x.has != 0 { Some(x.value) } else { None }
        };
        Ok((opt(&a), opt(&b), opt(&c)))
    }
    fn remaining(&self) -> usize { self.args.len() - self.pos }
}

/// Walk steps from the start of the value and deep-copy the resulting
/// value into a fresh SHM block. Entry point for non-bracket-only
/// paths (the BracketIndexOnly / BracketSliceOnly fast paths bypass
/// this).
pub(crate) fn walk_into_fresh(
    value_schema: &Schema,
    src: &SubpacketSrc,
    steps: &[WalkStep],
    args: &[crate::intrinsics::IFileWalkArg],
) -> Result<AbsPtr, MorlocError> {
    // Infer the result schema for the whole walk so we can allocate
    // the output once. infer_chain_schema is a pure static walk over
    // the input schema -- no disk reads.
    let result_schema = infer_chain_schema(value_schema, steps)?;
    let dst = shm::shcalloc(1, result_schema.width)?;
    let mut cursor = ArgsCursor::new(args);
    if let Err(e) = walk_into(src, src.arr_base(), value_schema,
                              steps, &mut cursor, dst, &result_schema) {
        let _ = shm::shfree(dst);
        return Err(e);
    }
    if cursor.remaining() != 0 {
        let _ = shm::shfree(dst);
        return Err(MorlocError::Other(format!(
            "walk: {} unconsumed runtime arg(s) -- pattern/args arity mismatch",
            cursor.remaining()
        )));
    }
    Ok(dst)
}

/// Walk `steps` from the current source position into a pre-allocated
/// output slot. Used both at the top level (`walk_into_fresh`
/// allocates and calls in) and recursively for group children
/// (`materialize_group` lays out the tuple and recurses per-child).
pub(crate) fn walk_into(
    src: &SubpacketSrc,
    cur_ptr: AbsPtr,
    cur_schema: &Schema,
    steps: &[WalkStep],
    args: &mut ArgsCursor<'_>,
    out: AbsPtr,
    out_schema: &Schema,
) -> Result<(), MorlocError> {
    // Pre-walk through any leading Field steps -- they are pure
    // pointer arithmetic and never consume args.
    let (mut cur_ptr, mut cur_schema_ref, mut tail) =
        navigate_field_prefix(cur_schema, cur_ptr, steps)?;

    loop {
        match tail.first() {
            None => {
                // Reached the end of the chain. Deep-copy current
                // value into out.
                debug_assert_eq!(out_schema.width, cur_schema_ref.width);
                return deep_copy_one(src, cur_ptr, cur_schema_ref, out);
            }
            Some(WalkStep::Field(_)) => unreachable!("navigate_field_prefix consumed all fields"),
            Some(WalkStep::BracketIndex) => {
                let idx_arg = args.next_one()?;
                if idx_arg.has == 0 {
                    return Err(MorlocError::Other(
                        "walk: bracket-index requires a present index (got None)".into(),
                    ));
                }
                // Read array header, bounds-check, advance cursor to
                // the element's source position, recurse on remainder.
                if cur_schema_ref.serial_type != SerialType::Array {
                    return Err(MorlocError::Other(format!(
                        "walk: bracket-index requires an Array, got {:?}",
                        cur_schema_ref.serial_type
                    )));
                }
                let (elem_ptr, elem_schema) =
                    locate_array_element(src, cur_ptr, cur_schema_ref, idx_arg.value)?;
                tail = &tail[1..];
                // Field-prefix-walk over any further field steps in
                // the chain; if `tail` is now Group/BracketSlice/...
                // the outer match handles it.
                let (np, ns, nt) = navigate_field_prefix(elem_schema, elem_ptr, tail)?;
                cur_ptr = np;
                cur_schema_ref = ns;
                tail = nt;
            }
            Some(WalkStep::BracketSlice) => {
                // Slice consumes 3 runtime args (start, stop, step).
                // Terminal slice deep-copies; slice-with-tail
                // broadcasts the tail over each element (matches the
                // compiler's IntrMap desugar).
                let (s, e, p) = args.next_three()?;
                if cur_schema_ref.serial_type != SerialType::Array {
                    return Err(MorlocError::Other(format!(
                        "walk: bracket-slice requires an Array, got {:?}",
                        cur_schema_ref.serial_type
                    )));
                }
                let remaining = &tail[1..];
                if remaining.is_empty() {
                    return inline_bracket_slice(
                        src, cur_ptr, cur_schema_ref, s, e, p, out,
                    );
                }
                return broadcast_slice_tail(
                    src, cur_ptr, cur_schema_ref, s, e, p,
                    remaining, args, out, out_schema,
                );
            }
            Some(WalkStep::Group(chains)) => {
                // Group is terminal by parse-time check; result writes
                // directly into `out`.
                debug_assert_eq!(tail.len(), 1, "Group should be terminal");
                return materialize_group(src, cur_ptr, cur_schema_ref,
                                          chains, args, out, out_schema);
            }
        }
    }
}

/// Consume Field steps from the front of `steps`. Stops at end-of-
/// list or at the first non-Field step. Pure pointer arithmetic; no
/// disk reads, no args.
pub(crate) fn navigate_field_prefix<'a>(
    start_schema: &'a Schema,
    start_ptr: AbsPtr,
    steps: &'a [WalkStep],
) -> Result<(AbsPtr, &'a Schema, &'a [WalkStep]), MorlocError> {
    let mut cur_ptr = start_ptr;
    let mut cur_schema = start_schema;
    let mut i = 0;
    while i < steps.len() {
        match &steps[i] {
            WalkStep::Field(f) => {
                let (np, ns) = step_field(cur_ptr, cur_schema, f, i)?;
                cur_ptr = np;
                cur_schema = ns;
                i += 1;
            }
            _ => break,
        }
    }
    Ok((cur_ptr, cur_schema, &steps[i..]))
}

pub(crate) fn step_field<'a>(
    cur_ptr: AbsPtr,
    cur_schema: &'a Schema,
    f: &FieldStep,
    step_i: usize,
) -> Result<(AbsPtr, &'a Schema), MorlocError> {
    let field_idx = match f {
        FieldStep::Index(i) => *i,
        FieldStep::Key(k) => cur_schema.keys.iter().position(|sk| sk == k).ok_or_else(|| {
            MorlocError::Other(format!(
                "field '{}' not found at step {} (schema keys = {:?})",
                k, step_i, cur_schema.keys
            ))
        })?,
    };
    if field_idx >= cur_schema.parameters.len() {
        return Err(MorlocError::Other(format!(
            "field index {} out of range at step {} (schema has {} params)",
            field_idx, step_i, cur_schema.parameters.len()
        )));
    }
    if field_idx >= cur_schema.offsets.len() {
        return Err(MorlocError::Other(format!(
            "schema offsets[{}] missing at step {}", field_idx, step_i
        )));
    }
    let off = cur_schema.offsets[field_idx];
    let next_ptr = unsafe { (cur_ptr as *const u8).add(off) as AbsPtr };
    Ok((next_ptr, &cur_schema.parameters[field_idx]))
}

/// Resolve `arr_ptr` as an Array struct, bounds-check `idx` against
/// `arr.size` (with Python negative-index semantics), and return the
/// element's source pointer + schema. Source-side only -- no copy.
pub(crate) fn locate_array_element<'a>(
    src: &SubpacketSrc,
    arr_ptr: AbsPtr,
    arr_schema: &'a Schema,
    idx: i64,
) -> Result<(AbsPtr, &'a Schema), MorlocError> {
    debug_assert_eq!(arr_schema.serial_type, SerialType::Array);
    if arr_schema.parameters.is_empty() {
        return Err(MorlocError::Other("walk: Array schema missing element type".into()));
    }
    let arr = unsafe { &*(arr_ptr as *const shm_types_crate::Array) };
    let actual = slice::resolve_array_index(idx, arr.size)
        .ok_or_else(|| MorlocError::Other(format!(
            "walk: bracket-index {} out of bounds (size {})", idx, arr.size
        )))?;
    let elem_schema = &arr_schema.parameters[0];
    let data_abs = src.array_data(arr, elem_schema.width)?;
    let elem_ptr = unsafe {
        (data_abs as *const u8).add(actual * elem_schema.width) as AbsPtr
    };
    Ok((elem_ptr, elem_schema))
}

/// Apply BracketSlice on an in-file Array. Writes the resulting
/// Array { size, RelPtr data } header into `out`; allocates a fresh
/// SHM block for the element bank and deep-copies each selected
/// element into it. Mirrors the semantics of `ifile_bracket_slice`
/// for the file's root array, but operates at an arbitrary
/// (ptr, schema) -- used inside group children whose chain ends in
/// `.[:]`.
pub(crate) fn inline_bracket_slice(
    src: &SubpacketSrc,
    arr_ptr: AbsPtr,
    arr_schema: &Schema,
    start: Option<i64>,
    stop: Option<i64>,
    step: Option<i64>,
    out: AbsPtr,
) -> Result<(), MorlocError> {
    debug_assert_eq!(arr_schema.serial_type, SerialType::Array);
    if arr_schema.parameters.is_empty() {
        return Err(MorlocError::Other("walk: Array schema missing element type".into()));
    }
    let arr = unsafe { &*(arr_ptr as *const shm_types_crate::Array) };
    let elem_schema = &arr_schema.parameters[0];
    let elem_w = elem_schema.width;
    let slice = slice::Slice::over_array(arr.size, start, stop, step)?;
    let n_out = width::usize_from_u64(slice.len());
    // Allocate the element bank. shcalloc handles n_out == 0 by
    // returning a sentinel; we still need to size the output header.
    let buf_ptr = if n_out == 0 {
        std::ptr::null::<u8>() as AbsPtr
    } else {
        shm::shcalloc(n_out, elem_w)?
    };
    let data_abs = src.array_data(arr, elem_w)?;
    for (k, i) in slice.positions().enumerate() {
        let elem_src = unsafe { (data_abs as *const u8).add(i * elem_w) as AbsPtr };
        let elem_dst = unsafe { (buf_ptr as *mut u8).add(k * elem_w) as AbsPtr };
        if let Err(e) = deep_copy_one(src, elem_src, elem_schema, elem_dst) {
            if !buf_ptr.is_null() { let _ = shm::shfree(buf_ptr); }
            return Err(e);
        }
    }
    // Write the Array { size, RelPtr data } header into out.
    let data_rel = if buf_ptr.is_null() {
        // Empty slice: write a relptr that resolves to a 0-byte
        // region. RELNULL is conventionally used to encode "no data".
        shm::RELNULL
    } else {
        shm::abs2rel(buf_ptr)?
    };
    let header = shm_types_crate::Array { size: n_out, data: data_rel };
    unsafe {
        std::ptr::copy_nonoverlapping(
            &header as *const shm_types_crate::Array as *const u8,
            out as *mut u8,
            std::mem::size_of::<shm_types_crate::Array>(),
        );
    }
    Ok(())
}

/// Slice with a tail chain: for each element of the sliced source,
/// walk `tail` on that element, and collect the results into a fresh
/// `Array<tail_result>` written to `out`. Mirrors the compiler's
/// IntrMap desugar (`.[:].tail` lowers to `map (\e -> e.tail) slice`).
///
/// `arr_schema` describes the input Array; `out_schema` describes the
/// output Array (`Array<tail_result_from_elem>`, produced by
/// `infer_chain_schema`).
///
/// Args semantics: the tail is walked once per output element; any
/// arg-consuming step in the tail (BracketIndex, nested BracketSlice)
/// consumes fresh args on each iteration, so the caller must supply
/// enough. In practice broadcast tails are field/tuple-idx / groups
/// with field-only children -- no runtime args -- but this
/// implementation doesn't restrict that.
pub(crate) fn broadcast_slice_tail(
    src: &SubpacketSrc,
    arr_ptr: AbsPtr,
    arr_schema: &Schema,
    start: Option<i64>,
    stop: Option<i64>,
    step: Option<i64>,
    tail: &[WalkStep],
    args: &mut ArgsCursor<'_>,
    out: AbsPtr,
    out_schema: &Schema,
) -> Result<(), MorlocError> {
    debug_assert_eq!(arr_schema.serial_type, SerialType::Array);
    debug_assert_eq!(out_schema.serial_type, SerialType::Array);
    if arr_schema.parameters.is_empty() {
        return Err(MorlocError::Other(
            "walk: Array schema missing element type at slice broadcast".into(),
        ));
    }
    if out_schema.parameters.is_empty() {
        return Err(MorlocError::Other(
            "walk: output Array schema missing element type at slice broadcast".into(),
        ));
    }
    let arr = unsafe { &*(arr_ptr as *const shm_types_crate::Array) };
    let elem_schema = &arr_schema.parameters[0];
    let elem_w = elem_schema.width;
    let out_elem_schema = &out_schema.parameters[0];
    let out_elem_w = out_elem_schema.width;
    let slice = slice::Slice::over_array(arr.size, start, stop, step)?;
    let n_out = width::usize_from_u64(slice.len());
    let buf_ptr = if n_out == 0 {
        std::ptr::null::<u8>() as AbsPtr
    } else {
        shm::shcalloc(n_out, out_elem_w)?
    };
    let data_abs = src.array_data(arr, elem_w)?;
    // The tail's runtime args (any BracketIndex/BracketSlice in the
    // tail) apply uniformly to every element of the slice, matching
    // `map (\e -> e.[i]) slice`. Snapshot the arg cursor, rewind
    // before each element, then advance once after the loop so the
    // outer caller sees a single consumption of the tail's args.
    let snapshot_pos = args.pos;
    let mut per_iter_consumed: Option<usize> = None;
    for (k, i) in slice.positions().enumerate() {
        let elem_src_ptr = unsafe {
            (data_abs as *const u8).add(i * elem_w) as AbsPtr
        };
        let elem_dst_ptr = unsafe {
            (buf_ptr as *mut u8).add(k * out_elem_w) as AbsPtr
        };
        args.pos = snapshot_pos;
        if let Err(e) = walk_into(
            src, elem_src_ptr, elem_schema,
            tail, args, elem_dst_ptr, out_elem_schema,
        ) {
            if !buf_ptr.is_null() { let _ = shm::shfree(buf_ptr); }
            return Err(e);
        }
        let consumed = args.pos - snapshot_pos;
        match per_iter_consumed {
            None => per_iter_consumed = Some(consumed),
            Some(prev) if prev != consumed => {
                if !buf_ptr.is_null() { let _ = shm::shfree(buf_ptr); }
                return Err(MorlocError::Other(format!(
                    "walk: broadcast tail consumed different arg counts \
                     across iterations ({} vs {}); pattern is malformed",
                    prev, consumed,
                )));
            }
            _ => {}
        }
    }
    // Advance the outer cursor once past the tail's arg budget.
    // For empty slices no iteration ran, so we compute the count
    // statically from the tail's step shapes instead of observing it.
    let per_iter = match per_iter_consumed {
        Some(c) => c,
        None => count_walk_args(tail),
    };
    args.pos = snapshot_pos + per_iter;
    let data_rel = if buf_ptr.is_null() {
        shm::RELNULL
    } else {
        shm::abs2rel(buf_ptr)?
    };
    let header = shm_types_crate::Array { size: n_out, data: data_rel };
    unsafe {
        std::ptr::copy_nonoverlapping(
            &header as *const shm_types_crate::Array as *const u8,
            out as *mut u8,
            std::mem::size_of::<shm_types_crate::Array>(),
        );
    }
    Ok(())
}

/// Count the number of runtime args a chain of WalkSteps would
/// consume. Each BracketIndex uses 1, each BracketSlice uses 3, Field
/// uses 0. Groups accumulate the counts of every child chain.
/// Used by `broadcast_slice_tail` to skip the tail's arg budget when
/// the slice is empty (no iteration to observe consumption).
pub(crate) fn count_walk_args(steps: &[WalkStep]) -> usize {
    let mut n = 0;
    for s in steps {
        match s {
            WalkStep::Field(_) => {}
            WalkStep::BracketIndex => n += 1,
            WalkStep::BracketSlice => n += 3,
            WalkStep::Group(children) => {
                for c in children {
                    n += count_walk_args(c);
                }
            }
        }
    }
    n
}

/// Materialise a group of sibling sub-walks into a tuple at `out`.
/// Each child writes directly into its slot.
pub(crate) fn materialize_group(
    src: &SubpacketSrc,
    cur_ptr: AbsPtr,
    cur_schema: &Schema,
    chains: &[Vec<WalkStep>],
    args: &mut ArgsCursor<'_>,
    out: AbsPtr,
    out_schema: &Schema,
) -> Result<(), MorlocError> {
    // Empty group: nothing to do. The output slot is `out_schema`'s
    // width (zero for Nil/empty-tuple); the caller's shcalloc has
    // already zeroed it.
    if chains.is_empty() {
        return Ok(());
    }
    debug_assert_eq!(out_schema.serial_type, SerialType::Tuple);
    debug_assert_eq!(out_schema.parameters.len(), chains.len());
    debug_assert_eq!(out_schema.offsets.len(), chains.len());
    for (i, chain) in chains.iter().enumerate() {
        let slot_off = out_schema.offsets[i];
        let slot_schema = &out_schema.parameters[i];
        let slot_dst = unsafe { (out as *mut u8).add(slot_off) as AbsPtr };
        walk_into(src, cur_ptr, cur_schema, chain, args, slot_dst, slot_schema)?;
    }
    Ok(())
}

/// Compute the static schema produced by walking `chain` from
/// `start`. Pure static walk -- no disk reads. Used to size the
/// output buffer up front and to lay out tuple slots for groups.
pub(crate) fn infer_chain_schema(
    start: &Schema,
    chain: &[WalkStep],
) -> Result<Schema, MorlocError> {
    let mut cur = start.clone();
    for (step_i, step) in chain.iter().enumerate() {
        match step {
            WalkStep::Field(f) => {
                let field_idx = match f {
                    FieldStep::Index(i) => *i,
                    FieldStep::Key(k) => cur.keys.iter().position(|sk| sk == k).ok_or_else(|| {
                        MorlocError::Other(format!(
                            "field '{}' not found at step {} during schema inference",
                            k, step_i
                        ))
                    })?,
                };
                if field_idx >= cur.parameters.len() {
                    return Err(MorlocError::Other(format!(
                        "field index {} out of range at step {} during schema inference",
                        field_idx, step_i
                    )));
                }
                cur = cur.parameters[field_idx].clone();
            }
            WalkStep::BracketIndex => {
                if cur.serial_type != SerialType::Array {
                    return Err(MorlocError::Other(format!(
                        "bracket-index at step {} requires an Array, got {:?}",
                        step_i, cur.serial_type
                    )));
                }
                if cur.parameters.is_empty() {
                    return Err(MorlocError::Other(
                        "Array schema missing element type".into(),
                    ));
                }
                cur = cur.parameters[0].clone();
            }
            WalkStep::BracketSlice => {
                if cur.serial_type != SerialType::Array {
                    return Err(MorlocError::Other(format!(
                        "bracket-slice at step {} requires an Array, got {:?}",
                        step_i, cur.serial_type
                    )));
                }
                // Terminal slice: shape unchanged. Slice with a tail:
                // broadcast the tail over each element, so the result
                // becomes `Array<tail_result_from_elem>`.
                let remaining = &chain[step_i + 1..];
                if !remaining.is_empty() {
                    if cur.parameters.is_empty() {
                        return Err(MorlocError::Other(
                            "Array schema missing element type at slice broadcast".into(),
                        ));
                    }
                    let elem = cur.parameters[0].clone();
                    let tail_result = infer_chain_schema(&elem, remaining)?;
                    return Ok(array_schema(&tail_result));
                }
                // Else: cur stays unchanged (terminal slice).
            }
            WalkStep::Group(inner_chains) => {
                if inner_chains.is_empty() {
                    // Empty group -> unit / Nil schema.
                    return Ok(Schema::primitive(SerialType::Nil));
                }
                let mut child_schemas = Vec::with_capacity(inner_chains.len());
                for c in inner_chains {
                    child_schemas.push(infer_chain_schema(&cur, c)?);
                }
                let (width, offsets) = tuple_layout(&child_schemas);
                return Ok(Schema {
                    serial_type: SerialType::Tuple,
                    size: child_schemas.len(),
                    width,
                    offsets,
                    hint: None,
                    parameters: child_schemas,
                    keys: Vec::new(),
                    name: None,
                });
            }
        }
    }
    Ok(cur)
}

/// Voidstar tuple layout: each field sits at the natural alignment of
/// its type, total width is rounded up to the max field alignment.
/// Mirrors `morloc_runtime_types::schema::calculate_tuple_layout`
/// (which is private to that crate).
pub(crate) fn tuple_layout(params: &[Schema]) -> (usize, Vec<usize>) {
    let mut offsets = Vec::with_capacity(params.len());
    let mut offset: usize = 0;
    let mut max_align: usize = 1;
    for p in params {
        let a = p.alignment();
        if a > max_align { max_align = a; }
        offset = (offset + a - 1) & !(a - 1);
        offsets.push(offset);
        offset += p.width;
    }
    let width = (offset + max_align - 1) & !(max_align - 1);
    (width, offsets)
}

/// Deep-copy a value of the sub-packet, resolving through its space.
pub(crate) fn deep_copy_one(
    src: &SubpacketSrc,
    cur_ptr: AbsPtr,
    cur_schema: &Schema,
    dst: AbsPtr,
) -> Result<(), MorlocError> {
    unsafe { voidstar::deep_copy_with(cur_ptr, dst, cur_schema, &src.space()) }
}

