//! Expression evaluator and constructor functions.
//! Replaces eval.c. Uses HashMap instead of linked-list dict_t.

use std::collections::HashMap;
use std::ffi::{c_char, c_void, CStr};
use std::ptr;

use crate::cschema::CSchema;
use crate::error::{clear_errmsg, set_errmsg, MorlocError};
use crate::manifest_ffi::*;
use crate::shm::{self, AbsPtr, RelPtr};

// ── Constructor functions (called by manifest_ffi.rs and daemon.c) ───────────

#[no_mangle]
pub unsafe extern "C" fn make_morloc_bound_var(
    schema_str: *const c_char,
    varname: *mut c_char,
    errmsg: *mut *mut c_char,
) -> *mut MorlocExpression {
    clear_errmsg(errmsg);
    let mut err: *mut c_char = ptr::null_mut();
    let schema = crate::ffi::parse_schema(schema_str, &mut err);
    if !err.is_null() { *errmsg = err; return ptr::null_mut(); }

    let expr = libc::calloc(1, std::mem::size_of::<MorlocExpression>()) as *mut MorlocExpression;
    if expr.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("Failed to allocate bound variable expression".into()));
        return ptr::null_mut();
    }
    (*expr).etype = MorlocExpressionType::Bnd;
    (*expr).schema = schema;
    (*expr).expr.bnd_expr = varname;
    expr
}

#[no_mangle]
pub unsafe extern "C" fn make_morloc_literal(
    schema_str: *const c_char,
    lit: Primitive,
    errmsg: *mut *mut c_char,
) -> *mut MorlocExpression {
    clear_errmsg(errmsg);
    let mut err: *mut c_char = ptr::null_mut();
    let schema = crate::ffi::parse_schema(schema_str, &mut err);
    if !err.is_null() { *errmsg = err; return ptr::null_mut(); }

    let data = libc::malloc(std::mem::size_of::<MorlocData>()) as *mut MorlocData;
    if data.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("Failed to allocate literal data".into()));
        return ptr::null_mut();
    }
    (*data).is_voidstar = false;
    (*data).data = DataUnion { lit_val: std::mem::ManuallyDrop::new(lit) };

    let expr = libc::malloc(std::mem::size_of::<MorlocExpression>()) as *mut MorlocExpression;
    if expr.is_null() {
        libc::free(data as *mut c_void);
        set_errmsg(errmsg, &MorlocError::Other("Failed to allocate literal expression".into()));
        return ptr::null_mut();
    }
    (*expr).etype = MorlocExpressionType::Dat;
    (*expr).schema = schema;
    (*expr).expr.data_expr = data;
    expr
}

#[no_mangle]
pub unsafe extern "C" fn make_morloc_pattern(
    schema_str: *const c_char,
    pattern: *mut MorlocPattern,
    errmsg: *mut *mut c_char,
) -> *mut MorlocExpression {
    clear_errmsg(errmsg);
    let mut err: *mut c_char = ptr::null_mut();
    let schema = crate::ffi::parse_schema(schema_str, &mut err);
    if !err.is_null() { *errmsg = err; return ptr::null_mut(); }

    let expr = libc::calloc(1, std::mem::size_of::<MorlocExpression>()) as *mut MorlocExpression;
    if expr.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("Failed to allocate pattern expression".into()));
        return ptr::null_mut();
    }
    (*expr).etype = MorlocExpressionType::Pat;
    (*expr).schema = schema;
    (*expr).expr.pattern_expr = pattern;
    expr
}

#[no_mangle]
pub extern "C" fn make_morloc_pattern_end() -> *mut MorlocPattern {
    // SAFETY: calloc returns zeroed memory suitable for MorlocPattern.
    // We initialize all fields before returning.
    unsafe {
        let pat = libc::calloc(1, std::mem::size_of::<MorlocPattern>()) as *mut MorlocPattern;
        if pat.is_null() { return ptr::null_mut(); }
        (*pat).ptype = MorlocPatternType::End;
        (*pat).size = 0;
        (*pat).fields = PatternFields { indices: ptr::null_mut() };
        (*pat).selectors = ptr::null_mut();
        pat
    }
}

// Varargs constructors (make_morloc_container, make_morloc_app,
// make_morloc_lambda, make_morloc_interpolation, make_morloc_pattern_idx,
// make_morloc_pattern_key) are only used by generated C++ pool code.
// They cannot be implemented in stable Rust due to C-variadic ABI.
// The C-gcc build path (morloc init) provides them from the original eval.c.
// The Rust hybrid build does not call them (only morloc_eval and the
// non-varargs constructors are needed).

// ── BigInt helpers ───────────────────────────────────────────────────────────

/// Convert a decimal string (possibly with leading '-') to a Vec of uint64
/// limbs in little-endian order using two's complement for negative values.
/// For values that fit in i64 (the common case), produces exactly 1 limb.
///
/// Strict: every non-sign byte must be an ASCII digit. The empty string and
/// `-` alone are rejected. Callers that want lenient parsing should
/// pre-strip whitespace/quotes themselves.
pub fn decimal_to_limbs(s: &str) -> Result<Vec<u64>, MorlocError> {
    let s = s.trim();
    if s.is_empty() {
        return Err(MorlocError::Serialization("empty integer literal".into()));
    }
    if s == "0" || s == "-0" {
        return Ok(vec![0u64]);
    }

    let (negative, digits) = if let Some(rest) = s.strip_prefix('-') {
        (true, rest)
    } else {
        (false, s)
    };

    if digits.is_empty() {
        return Err(MorlocError::Serialization("integer literal has no digits".into()));
    }
    if !digits.bytes().all(|b| b.is_ascii_digit()) {
        return Err(MorlocError::Serialization(format!(
            "invalid integer literal: {:?} (expected decimal digits)", s
        )));
    }

    // Parse magnitude into limbs (base 2^64, little-endian)
    let mut limbs: Vec<u64> = vec![0];
    for &b in digits.as_bytes() {
        let d = (b - b'0') as u64;
        let mut carry = d;
        for limb in limbs.iter_mut() {
            let wide = (*limb as u128) * 10 + carry as u128;
            *limb = wide as u64;
            carry = (wide >> 64) as u64;
        }
        if carry > 0 {
            limbs.push(carry);
        }
    }

    if negative {
        // Two's complement: invert all bits, then add 1
        let mut carry: u64 = 1;
        for limb in limbs.iter_mut() {
            *limb = !*limb;
            let (sum, c) = limb.overflowing_add(carry);
            *limb = sum;
            carry = if c { 1 } else { 0 };
        }
        // For negative numbers, extend with 0xFFFF... if high bit is not set
        if limbs.last().map_or(false, |&l| l >> 63 == 0) {
            limbs.push(u64::MAX);
        }
    } else {
        // For positive numbers, extend with 0 if high bit is set (would look negative)
        if limbs.last().map_or(false, |&l| l >> 63 != 0) {
            limbs.push(0);
        }
    }

    Ok(limbs)
}

/// Convert BigInt limbs (little-endian, two's complement) back to decimal string.
pub fn limbs_to_decimal(limbs: &[u64]) -> String {
    if limbs.is_empty() {
        return "0".to_string();
    }

    // Check sign via high bit of most significant limb
    let negative = limbs.last().map_or(false, |&l| l >> 63 != 0);

    // Get magnitude limbs
    let mag = if negative {
        // Negate: invert + add 1
        let mut m: Vec<u64> = limbs.iter().map(|&l| !l).collect();
        let mut carry: u64 = 1;
        for limb in m.iter_mut() {
            let (sum, c) = limb.overflowing_add(carry);
            *limb = sum;
            carry = if c { 1 } else { 0 };
        }
        m
    } else {
        limbs.to_vec()
    };

    // Convert to decimal by repeated division by 10
    let mut digits: Vec<u8> = Vec::new();
    let mut work = mag;
    loop {
        // Strip leading zero limbs
        while work.len() > 1 && *work.last().unwrap() == 0 {
            work.pop();
        }
        if work.len() == 1 && work[0] == 0 {
            break;
        }
        // Divide work by 10, collect remainder
        let mut remainder: u128 = 0;
        for limb in work.iter_mut().rev() {
            let val = (remainder << 64) | (*limb as u128);
            *limb = (val / 10) as u64;
            remainder = val % 10;
        }
        digits.push(remainder as u8 + b'0');
    }

    if digits.is_empty() {
        return "0".to_string();
    }
    digits.reverse();
    let s = String::from_utf8(digits).unwrap_or_else(|_| "0".to_string());
    if negative { format!("-{}", s) } else { s }
}

// ── Core evaluator ───────────────────────────────────────────────────────────

type BndVars<'a> = HashMap<&'a str, AbsPtr>;

/// Convert key-based pattern selectors to index-based using the schema's key names.
///
/// Walks top-down so that each child pattern is recursed with the schema
/// it actually corresponds to (`schema.parameters[converted_index]`),
/// rather than with the schema parameter at the *pattern's* positional
/// index. The latter was the pre-fix behaviour and silently mismatched
/// child schemas in chained nested patterns like `.a.b r`, dereferencing
/// a null `keys` pointer when the wrong child schema was a primitive
/// (e.g. Str) whose `keys` field is null.
///
/// # Safety
/// `pattern` and `schema` must be valid, non-null pointers to C-allocated structures.
/// `schema` keys array must have `schema.size` entries.
unsafe fn convert_keys_to_indices(
    pattern: *mut MorlocPattern,
    schema: *const CSchema,
) -> Result<(), MorlocError> {
    let pat = &mut *pattern;
    let n_params = (*schema).size;

    // Convert THIS level's keys to indices first, so child recursion below
    // can pick the correct child schema from `schema.parameters`.
    if pat.ptype == MorlocPatternType::ByKey {
        let indices = libc::calloc(pat.size, std::mem::size_of::<usize>()) as *mut usize;
        for i in 0..pat.size {
            let key = CStr::from_ptr(*pat.fields.keys.add(i)).to_str().unwrap_or("");
            let mut found = false;
            for j in 0..n_params {
                let record_key = CStr::from_ptr(*(*schema).keys.add(j)).to_str().unwrap_or("");
                if key == record_key {
                    found = true;
                    *indices.add(i) = j;
                    break;
                }
            }
            if !found {
                libc::free(indices as *mut c_void);
                return Err(MorlocError::Other(format!("Pattern contains key missing in schema: {}", key)));
            }
            libc::free(*pat.fields.keys.add(i) as *mut c_void);
        }
        pat.ptype = MorlocPatternType::ByIndex;
        libc::free(pat.fields.keys as *mut c_void);
        pat.fields.indices = indices;
    }

    // Now recurse into each child using the *converted* index to pick
    // the right child schema (instead of `parameters.add(i)`, which would
    // index by pattern position).
    if pat.ptype == MorlocPatternType::ByIndex {
        for i in 0..pat.size {
            let idx = *pat.fields.indices.add(i);
            if idx >= n_params {
                return Err(MorlocError::Other(format!(
                    "Pattern child index {} out of range for schema size {}", idx, n_params
                )));
            }
            let child_schema = *(*schema).parameters.add(idx);
            convert_keys_to_indices(*pat.selectors.add(i), child_schema)?;
        }
    }

    Ok(())
}

/// Count the number of terminal (End) nodes in a pattern. The return schema
/// is a tuple-of-terminals only when this count exceeds one; with a single
/// terminal the return schema IS the terminal's value type, even if that
/// type is itself a tuple.
unsafe fn count_pattern_terminals(pattern: *mut MorlocPattern) -> usize {
    let pat = &*pattern;
    match pat.ptype {
        MorlocPatternType::End => 1,
        MorlocPatternType::ByIndex | MorlocPatternType::ByKey => {
            let mut total = 0;
            for i in 0..pat.size {
                total += count_pattern_terminals(*pat.selectors.add(i));
            }
            total
        }
        // Bracket-index in chain mode (size=1, with sub) counts the
        // sub's terminals; in leaf mode (size=0, standalone) it is
        // not part of a getter walk and contributes 0.
        MorlocPatternType::BracketIndex => {
            if pat.size > 0 {
                count_pattern_terminals(*pat.selectors.add(0))
            } else {
                0
            }
        }
        // Bracket-slice is terminal (returns a list); contributes 1
        // terminal whether in chain mode or as a standalone App leaf.
        MorlocPatternType::BracketSlice => 1,
    }
}

/// DFS-count of bracket runtime args a pattern consumes when applied
/// as the function of a unified PatternStruct App. Bracket-index
/// chain steps contribute 1 (the index); bracket-slice (always
/// terminal) contributes 3 (start, stop, step).
unsafe fn count_bracket_arity(pattern: *mut MorlocPattern) -> usize {
    let pat = &*pattern;
    match pat.ptype {
        MorlocPatternType::End => 0,
        MorlocPatternType::BracketIndex => {
            let sub = if pat.size > 0 {
                count_bracket_arity(*pat.selectors.add(0))
            } else {
                0
            };
            1 + sub
        }
        MorlocPatternType::BracketSlice => 3,
        MorlocPatternType::ByIndex | MorlocPatternType::ByKey => {
            let mut total = 0;
            for i in 0..pat.size {
                total += count_bracket_arity(*pat.selectors.add(i));
            }
            total
        }
    }
}

/// Extract fields from a voidstar value using a pattern, copying them into dest.
///
/// Supports bracket steps inside the selector chain: when the pattern
/// reaches a 'BracketIndex' node with size=1 (chain mode) or
/// 'BracketSlice' (always terminal), the walker consumes 1 (index)
/// or 3 (start, stop, step) runtime args from `bracket_args` /
/// `bracket_schemas` in DFS order via `bracket_cursor`. Leaf-mode
/// bracket patterns (PatternBracketIndex / PatternBracketSlice used
/// as standalone App functions) reach this walker via the App
/// dispatch which passes them empty bracket-args; a leaf bracket
/// encountered here is an upstream error.
///
/// # Safety
/// All pointer arguments must be valid and point to correctly-typed C structures.
/// `value` must point to voidstar data matching `value_schema`.
unsafe fn apply_getter(
    dest: AbsPtr,
    return_index: &mut usize,
    return_schema: *const CSchema,
    multi_terminal: bool,
    pattern: *mut MorlocPattern,
    value_schema: *const CSchema,
    value: AbsPtr,
    bracket_args: &[AbsPtr],
    bracket_schemas: &[*mut CSchema],
    bracket_cursor: &mut usize,
) -> Result<AbsPtr, MorlocError> {
    let pat = &*pattern;

    match pat.ptype {
        MorlocPatternType::ByIndex => {
            for i in 0..pat.size {
                let idx = *pat.fields.indices.add(i);
                apply_getter(
                    dest, return_index, return_schema, multi_terminal,
                    *pat.selectors.add(i),
                    *(*value_schema).parameters.add(idx),
                    value.add(*(*value_schema).offsets.add(idx)),
                    bracket_args, bracket_schemas, bracket_cursor,
                )?;
            }
        }
        MorlocPatternType::ByKey => {
            convert_keys_to_indices(pattern, value_schema)?;
            return apply_getter(
                dest, return_index, return_schema, multi_terminal,
                pattern, value_schema, value,
                bracket_args, bracket_schemas, bracket_cursor,
            );
        }
        MorlocPatternType::End => {
            // With multiple terminals the return schema is a tuple of those
            // terminal values: write into the slot at `return_index`. With a
            // single terminal the return schema IS the terminal's value type
            // (possibly itself a tuple), so write the whole value into dest.
            let (element_dest, element_width) = if multi_terminal {
                (dest.add(*(*return_schema).offsets.add(*return_index)),
                 (*(*(*return_schema).parameters.add(*return_index))).width)
            } else {
                (dest, (*return_schema).width)
            };
            *return_index += 1;
            ptr::copy_nonoverlapping(value, element_dest, element_width);
        }
        MorlocPatternType::BracketIndex => {
            // Chain-mode bracket-index step: consume the next runtime arg
            // (the index), bounds-check, resolve the array's data and
            // compute the element source, then recurse on selectors[0]
            // with the element as the new value.
            if pat.size != 1 || pat.selectors.is_null() {
                return Err(MorlocError::Other(
                    "BracketIndex chain step requires exactly one sub-selector \
                     (size=1); leaf-mode bracket patterns dispatch via App".into(),
                ));
            }
            if *bracket_cursor >= bracket_args.len() {
                return Err(MorlocError::Other(
                    "apply_getter: bracket-index step exhausted runtime args".into(),
                ));
            }
            let idx_ptr = bracket_args[*bracket_cursor];
            let idx_schema = bracket_schemas[*bracket_cursor] as *const CSchema;
            *bracket_cursor += 1;

            let elem_schema = *(*value_schema).parameters as *const CSchema;
            let elem_width = (*elem_schema).width;
            let arr = &*(value as *const shm::Array);
            let n = arr.size as i64;

            let idx_raw = read_optional_int(idx_ptr, idx_schema)?
                .ok_or_else(|| MorlocError::Other(
                    "Bracket index cannot be Null".into()
                ))?;
            let idx = if idx_raw < 0 { idx_raw + n } else { idx_raw };
            if idx < 0 || idx >= n {
                return Err(MorlocError::Other(format!(
                    "Bracket index {} out of bounds for array of size {}",
                    idx_raw, n
                )));
            }
            let arr_data = shm::rel2abs(arr.data)?;
            let src_elem = arr_data.add(idx as usize * elem_width);

            return apply_getter(
                dest, return_index, return_schema, multi_terminal,
                *pat.selectors.add(0),
                elem_schema, src_elem,
                bracket_args, bracket_schemas, bracket_cursor,
            );
        }
        MorlocPatternType::BracketSlice => {
            // Terminal bracket-slice step: consume three runtime args
            // and write the resulting Array directly into the (multi-
            // terminal slot or whole) dest. The slice walker is the
            // same as the standalone PatternBracketSlice dispatcher;
            // we just re-use it.
            if *bracket_cursor + 3 > bracket_args.len() {
                return Err(MorlocError::Other(
                    "apply_getter: bracket-slice step needs 3 runtime args".into(),
                ));
            }
            let start_ptr = bracket_args[*bracket_cursor];
            let start_schema = bracket_schemas[*bracket_cursor] as *const CSchema;
            let stop_ptr = bracket_args[*bracket_cursor + 1];
            let stop_schema = bracket_schemas[*bracket_cursor + 1] as *const CSchema;
            let step_ptr = bracket_args[*bracket_cursor + 2];
            let step_schema = bracket_schemas[*bracket_cursor + 2] as *const CSchema;
            *bracket_cursor += 3;

            let (slot_dest, slot_width) = if multi_terminal {
                (dest.add(*(*return_schema).offsets.add(*return_index)),
                 (*(*(*return_schema).parameters.add(*return_index))).width)
            } else {
                (dest, (*return_schema).width)
            };
            *return_index += 1;

            apply_bracket_slice(
                slot_dest, slot_width, return_schema,
                start_ptr, start_schema,
                stop_ptr,  stop_schema,
                step_ptr,  step_schema,
                value,     value_schema,
            )?;
        }
    }

    Ok(dest)
}

/// Copy value into dest, preserving fields not selected by pattern.
///
/// # Safety
/// All pointer arguments must be valid. Schema sizes must match.
unsafe fn apply_setter_copy(
    dest: AbsPtr,
    return_schema: *const CSchema,
    pattern: *mut MorlocPattern,
    value_schema: *const CSchema,
    value: AbsPtr,
) -> Result<(), MorlocError> {
    let pat = &*pattern;
    match pat.ptype {
        MorlocPatternType::ByKey => {
            convert_keys_to_indices(pattern, value_schema)?;
            return apply_setter_copy(dest, return_schema, pattern, value_schema, value);
        }
        MorlocPatternType::ByIndex => {
            if (*value_schema).size != (*return_schema).size {
                return Err(MorlocError::Other("Expected setter return and input sizes to be the same".into()));
            }
            for i in 0..(*value_schema).size {
                let new_dest = dest.add(*(*return_schema).offsets.add(i));
                let new_value = value.add(*(*value_schema).offsets.add(i));
                let mut changed = false;
                for j in 0..pat.size {
                    if i == *pat.fields.indices.add(j) {
                        apply_setter_copy(
                            new_dest,
                            *(*return_schema).parameters.add(i),
                            *pat.selectors.add(j),
                            *(*value_schema).parameters.add(i),
                            new_value,
                        )?;
                        changed = true;
                        break;
                    }
                }
                if !changed {
                    let w = (*(*(*value_schema).parameters.add(i))).width;
                    ptr::copy_nonoverlapping(new_value, new_dest, w);
                }
            }
        }
        MorlocPatternType::End => {}
        MorlocPatternType::BracketIndex | MorlocPatternType::BracketSlice => {
            return Err(MorlocError::Other(
                "Bracket pattern reached apply_setter_copy; should have been dispatched earlier".into()
            ));
        }
    }
    Ok(())
}

/// Overwrite pattern-selected fields in dest with provided set_values.
///
/// # Safety
/// All pointer arguments must be valid. set_values must have enough entries.
unsafe fn apply_setter_set(
    dest: AbsPtr,
    return_schema: *const CSchema,
    pattern: *mut MorlocPattern,
    value_schema: *const CSchema,
    value: AbsPtr,
    set_schemas: *mut *mut CSchema,
    set_values: *mut AbsPtr,
    set_idx: &mut usize,
) -> Result<(), MorlocError> {
    let pat = &*pattern;
    match pat.ptype {
        MorlocPatternType::ByIndex => {
            for pi in 0..pat.size {
                let di = *pat.fields.indices.add(pi);
                apply_setter_set(
                    dest.add(*(*return_schema).offsets.add(di)),
                    *(*return_schema).parameters.add(di),
                    *pat.selectors.add(pi),
                    *(*value_schema).parameters.add(di),
                    value.add(*(*value_schema).offsets.add(di)),
                    set_schemas, set_values, set_idx,
                )?;
            }
        }
        MorlocPatternType::End => {
            ptr::copy_nonoverlapping(*set_values.add(*set_idx), dest, (*return_schema).width);
            *set_idx += 1;
        }
        MorlocPatternType::ByKey => {
            return Err(MorlocError::Other("Key patterns should have been resolved in copy step".into()));
        }
        MorlocPatternType::BracketIndex | MorlocPatternType::BracketSlice => {
            return Err(MorlocError::Other(
                "Bracket pattern reached apply_setter_set; should have been dispatched earlier".into()
            ));
        }
    }
    Ok(())
}

/// Read an integer value from a typed buffer, casting to i64. Returns
/// an error for non-integer schemas. BigInt values whose decimal does
/// not fit in i64 (more than 1 limb) also error -- bracket indices
/// have no semantically valid representation beyond i64 range.
unsafe fn read_int_as_i64(
    ptr: AbsPtr,
    schema: *const CSchema,
) -> Result<i64, MorlocError> {
    use crate::schema::SerialType;
    let stype = (*schema).serial_type;
    if stype == SerialType::Sint8 as u32 {
        Ok(*(ptr as *const i8) as i64)
    } else if stype == SerialType::Sint16 as u32 {
        Ok(*(ptr as *const i16) as i64)
    } else if stype == SerialType::Sint32 as u32 {
        Ok(*(ptr as *const i32) as i64)
    } else if stype == SerialType::Sint64 as u32 {
        Ok(*(ptr as *const i64))
    } else if stype == SerialType::Uint8 as u32 {
        Ok(*(ptr as *const u8) as i64)
    } else if stype == SerialType::Uint16 as u32 {
        Ok(*(ptr as *const u16) as i64)
    } else if stype == SerialType::Uint32 as u32 {
        Ok(*(ptr as *const u32) as i64)
    } else if stype == SerialType::Uint64 as u32 {
        Ok(*(ptr as *const u64) as i64)
    } else if stype == SerialType::Int as u32 {
        // Inline BigInt: [size, value_or_relptr]. Sizes 0 and 1 fit i64.
        let size = *(ptr as *const usize);
        if size == 0 {
            Ok(0)
        } else if size == 1 {
            let off = std::mem::size_of::<usize>();
            Ok(*(ptr.add(off) as *const i64))
        } else {
            Err(MorlocError::Other(
                "Bracket bound: BigInt with more than 1 limb does not fit in i64".into()
            ))
        }
    } else if stype == SerialType::IFile as u32
        || stype == SerialType::OStream as u32
        || stype == SerialType::IStream as u32
    {
        // Stream-handle field: the codec branches on the tag byte
        // (TAG_PATH -> open the path locally, TAG_HANDLE -> use the
        // slot id directly). `kind` maps the schema code back to the
        // runtime's MLC_KIND_* so a TAG_PATH decode opens with the
        // right morloc-level type.
        let kind = if stype == SerialType::IFile as u32 {
            morloc_runtime_types::packet::MLC_KIND_IFILE
        } else if stype == SerialType::OStream as u32 {
            morloc_runtime_types::packet::MLC_KIND_OSTREAM
        } else {
            morloc_runtime_types::packet::MLC_KIND_ISTREAM
        };
        let mut err: *mut c_char = ptr::null_mut();
        let handle = crate::intrinsics::mlc_read_stream_field(
            ptr as *const std::ffi::c_void,
            ptr::null(),
            kind,
            &mut err,
        );
        if handle < 0 {
            let msg = if !err.is_null() {
                let m = CStr::from_ptr(err).to_string_lossy().into_owned();
                libc::free(err as *mut c_void);
                m
            } else {
                "mlc_read_stream_field returned -1".to_string()
            };
            return Err(MorlocError::Other(msg));
        }
        Ok(handle)
    } else {
        Err(MorlocError::Other(format!(
            "Bracket bound has non-integer serial type {}", stype
        )))
    }
}

/// Write an i64 into a destination slot whose schema is one of the
/// fixed-width integer types or the inline BigInt `Int`. For the
/// BigInt layout, we emit `[size=1, value=n]` (16 bytes) when n is
/// non-zero, or `[size=0, 0]` when n is zero -- both fit the inline
/// shape, no overflow relptr needed.
///
/// Used by intrinsic eval handlers (currently `@flen`) whose runtime
/// return type is the user-facing `Int` -- writing only the low 8
/// bytes would leave the high 8 bytes interpreted as a relptr to
/// overflow limbs and the JSON encoder would dereference garbage.
unsafe fn write_int_to_dest(
    dest: AbsPtr,
    schema: *const CSchema,
    n: i64,
) -> Result<(), MorlocError> {
    use crate::schema::SerialType;
    let stype = (*schema).serial_type;
    if stype == SerialType::Int as u32 {
        let size: usize = if n == 0 { 0 } else { 1 };
        ptr::copy_nonoverlapping(
            &size as *const usize as *const u8,
            dest,
            std::mem::size_of::<usize>(),
        );
        ptr::copy_nonoverlapping(
            &n as *const i64 as *const u8,
            dest.add(std::mem::size_of::<usize>()),
            std::mem::size_of::<i64>(),
        );
        Ok(())
    } else if stype == SerialType::Sint64 as u32 || stype == SerialType::Uint64 as u32 {
        ptr::copy_nonoverlapping(
            &n as *const i64 as *const u8,
            dest,
            std::mem::size_of::<i64>(),
        );
        Ok(())
    } else if stype == SerialType::Sint32 as u32 {
        let v = n as i32;
        ptr::copy_nonoverlapping(&v as *const i32 as *const u8, dest, 4);
        Ok(())
    } else if stype == SerialType::Uint32 as u32 {
        let v = n as u32;
        ptr::copy_nonoverlapping(&v as *const u32 as *const u8, dest, 4);
        Ok(())
    } else if stype == SerialType::Sint16 as u32 {
        let v = n as i16;
        ptr::copy_nonoverlapping(&v as *const i16 as *const u8, dest, 2);
        Ok(())
    } else if stype == SerialType::Uint16 as u32 {
        let v = n as u16;
        ptr::copy_nonoverlapping(&v as *const u16 as *const u8, dest, 2);
        Ok(())
    } else if stype == SerialType::Sint8 as u32 {
        *(dest as *mut i8) = n as i8;
        Ok(())
    } else if stype == SerialType::Uint8 as u32 {
        *(dest as *mut u8) = n as u8;
        Ok(())
    } else {
        Err(MorlocError::Other(format!(
            "write_int_to_dest: schema type {} is not an integer", stype,
        )))
    }
}

/// Read an optional integer slot. RELNULL (or empty Optional schema)
/// returns `None`; a present value returns `Some(read_int_as_i64)`.
/// Non-Optional integer schemas pass through to `read_int_as_i64`.
unsafe fn read_optional_int(
    ptr: AbsPtr,
    schema: *const CSchema,
) -> Result<Option<i64>, MorlocError> {
    use crate::schema::SerialType;
    if (*schema).serial_type == SerialType::Optional as u32 {
        let relptr = *(ptr as *const shm::RelPtr);
        if relptr == shm::RELNULL || (*schema).size == 0 {
            return Ok(None);
        }
        let inner_abs = shm::rel2abs(relptr)?;
        let inner_schema = *(*schema).parameters as *const CSchema;
        Ok(Some(read_int_as_i64(inner_abs, inner_schema)?))
    } else {
        Ok(Some(read_int_as_i64(ptr, schema)?))
    }
}

/// Bracket-index evaluator: `arr[i]`. Reads `i` as an integer (any
/// integer wire type; negative wraps from the end), bounds-checks, and
/// deep-copies the selected element into `dest`. The receiver must be
/// an Array; the result schema is the element schema.
unsafe fn apply_bracket_index(
    dest: AbsPtr,
    _result_schema: *const CSchema,
    idx_ptr: AbsPtr,
    idx_schema: *const CSchema,
    arr_ptr: AbsPtr,
    arr_schema: *const CSchema,
) -> Result<(), MorlocError> {
    let elem_schema = *(*arr_schema).parameters as *const CSchema;
    let elem_width = (*elem_schema).width;
    let arr = &*(arr_ptr as *const shm::Array);
    let n = arr.size as i64;

    let idx_raw = read_optional_int(idx_ptr, idx_schema)?
        .ok_or_else(|| MorlocError::Other("Bracket index cannot be Null".into()))?;
    let idx = if idx_raw < 0 { idx_raw + n } else { idx_raw };
    if idx < 0 || idx >= n {
        return Err(MorlocError::Other(format!(
            "Bracket index {} out of bounds for array of size {}",
            idx_raw, n
        )));
    }

    let arr_data = shm::rel2abs(arr.data)?;
    let src_elem = arr_data.add(idx as usize * elem_width);
    let elem_rs = crate::cschema::CSchema::to_rust(elem_schema);
    crate::voidstar::deep_copy(src_elem, dest, &elem_rs)?;
    Ok(())
}

/// Bracket-slice evaluator: `arr[start:stop:step]`. Each bound is
/// optional; defaults are decided by step sign (Python semantics).
/// Negative bounds wrap from the end. Step 0 is a runtime error.
/// Allocates a fresh output Array of `n_out * elem_width` bytes,
/// deep-copying each selected element, and writes the Array header
/// (size+relptr) into `dest`.
unsafe fn apply_bracket_slice(
    dest: AbsPtr,
    width: usize,
    _result_schema: *const CSchema,
    start_ptr: AbsPtr,
    start_schema: *const CSchema,
    stop_ptr: AbsPtr,
    stop_schema: *const CSchema,
    step_ptr: AbsPtr,
    step_schema: *const CSchema,
    arr_ptr: AbsPtr,
    arr_schema: *const CSchema,
) -> Result<(), MorlocError> {
    let elem_schema = *(*arr_schema).parameters as *const CSchema;
    let elem_width = (*elem_schema).width;
    let arr = &*(arr_ptr as *const shm::Array);
    let n = arr.size as i64;

    let start_raw = read_optional_int(start_ptr, start_schema)?;
    let stop_raw  = read_optional_int(stop_ptr,  stop_schema)?;
    let step_raw  = read_optional_int(step_ptr,  step_schema)?;

    let step: i64 = step_raw.unwrap_or(1);
    if step == 0 {
        return Err(MorlocError::Other("Bracket slice step cannot be 0".into()));
    }

    // Python-style normalization: negative values wrap from the end.
    // Defaults depend on step direction; explicit user values are
    // normalized and clamped, so the in-band sentinel -1 (default
    // exclusive lower bound for negative step) is distinguishable
    // from an explicit -1 (which wraps to n-1).
    let normalize = |v: i64| if v < 0 { v + n } else { v };
    let clamp = |v: i64| -> i64 {
        if step > 0 {
            v.max(0).min(n)
        } else {
            v.max(-1).min(n - 1)
        }
    };
    let start_norm: i64 = match start_raw {
        Some(v) => clamp(normalize(v)),
        None    => if step > 0 { 0 } else { n - 1 },
    };
    let stop_norm: i64 = match stop_raw {
        Some(v) => clamp(normalize(v)),
        None    => if step > 0 { n } else { -1 },
    };

    let mut indices: Vec<i64> = Vec::new();
    let mut i = start_norm;
    if step > 0 {
        while i < stop_norm {
            indices.push(i);
            i += step;
        }
    } else {
        while i > stop_norm {
            indices.push(i);
            i += step;
        }
    }
    let out_size = indices.len();

    let out_relptr: shm::RelPtr = if out_size == 0 {
        shm::RELNULL
    } else {
        let arr_data = shm::rel2abs(arr.data)?;
        let buf = shm::shcalloc(out_size, elem_width)?;
        let elem_rs = crate::cschema::CSchema::to_rust(elem_schema);
        for (out_i, &src_idx) in indices.iter().enumerate() {
            let src_elem = arr_data.add(src_idx as usize * elem_width);
            let dst_elem = buf.add(out_i * elem_width);
            crate::voidstar::deep_copy(src_elem, dst_elem, &elem_rs)?;
        }
        shm::abs2rel(buf)?
    };

    let header = shm::Array { size: out_size, data: out_relptr };
    ptr::copy_nonoverlapping(
        &header as *const shm::Array as *const u8,
        dest,
        width,
    );
    Ok(())
}

/// Recursively evaluate a morloc expression, writing results into SHM.
///
/// # Safety
/// `expr` must be a valid MorlocExpression pointer (or null for error).
/// If `dest` is non-null, it must point to `width` bytes of writable SHM.
unsafe fn morloc_eval_r(
    expr: *mut MorlocExpression,
    dest: AbsPtr,
    width: usize,
    bndvars: &mut BndVars,
) -> Result<AbsPtr, MorlocError> {
    if expr.is_null() {
        return Err(MorlocError::Other("Empty expression".into()));
    }

    let schema = (*expr).schema;
    let (dest, width) = if dest.is_null() {
        let w = (*schema).width;
        let d = shm::shcalloc(1, w)?;
        (d, w)
    } else {
        if width != (*schema).width {
            return Err(MorlocError::Other("Unexpected data size".into()));
        }
        (dest, width)
    };

    match (*expr).etype {
        MorlocExpressionType::Dat => {
            let data = (*expr).expr.data_expr;
            if (*data).is_voidstar {
                return Ok((*data).data.voidstar as AbsPtr);
            }

            let stype = (*schema).serial_type;
            if stype == crate::schema::SerialType::String as u32 {
                // String: allocate in SHM. Str literals are stored in the
                // length-aware `str` field of Primitive so that interior
                // NUL bytes survive (strlen would truncate at the first
                // NUL). The BigInt "j" path still uses `s` as a C-string;
                // see the Int branch below.
                let ms = std::mem::ManuallyDrop::into_inner(
                    ptr::read(&(*data).data.lit_val)
                ).str;
                let (str_size, str_ptr) = if ms.is_null() {
                    (0usize, ptr::null::<u8>())
                } else {
                    ((*ms).size, (*ms).data as *const u8)
                };
                let str_relptr: RelPtr = if str_size > 0 {
                    let abs = shm::shmemcpy(str_ptr, str_size)?;
                    shm::abs2rel(abs)?
                } else {
                    -1isize as RelPtr
                };
                let arr = shm::Array { size: str_size, data: str_relptr };
                ptr::copy_nonoverlapping(&arr as *const shm::Array as *const u8, dest, width);
            } else if stype == crate::schema::SerialType::Array as u32 {
                let arr_data = (*data).data.array_val;
                let arr_size = (*arr_data).size;
                let elem_schema = (*arr_data).schema;
                let elem_width = (*elem_schema).width;
                let arr_reldata: RelPtr = if arr_size > 0 {
                    let arr_abs = shm::shcalloc(arr_size, elem_width)?;
                    for i in 0..arr_size {
                        morloc_eval_r(
                            *(*arr_data).values.add(i),
                            arr_abs.add(i * elem_width),
                            elem_width,
                            bndvars,
                        )?;
                    }
                    shm::abs2rel(arr_abs)?
                } else {
                    -1isize as RelPtr
                };
                let arr = shm::Array { size: arr_size, data: arr_reldata };
                ptr::copy_nonoverlapping(&arr as *const shm::Array as *const u8, dest, width);
            } else if stype == crate::schema::SerialType::Tuple as u32
                   || stype == crate::schema::SerialType::Map as u32 {
                for i in 0..(*schema).size {
                    let elem_width = (*(*(*schema).parameters.add(i))).width;
                    let elem_dest = dest.add(*(*schema).offsets.add(i));
                    let element = *(*data).data.tuple_val.add(i);
                    morloc_eval_r(element, elem_dest, elem_width, bndvars)?;
                }
            } else if stype == crate::schema::SerialType::Optional as u32 {
                // Optional is a single relptr (RELNULL = Nothing).
                //   tuple_val == null  -> Nothing: write RELNULL into slot.
                //   tuple_val != null  -> Just: allocate inner T in SHM,
                //                         recurse to populate it, store the
                //                         resulting relptr in the slot.
                let tv = (*data).data.tuple_val;
                let slot = dest as *mut shm::RelPtr;
                if tv.is_null() {
                    *slot = shm::RELNULL;
                } else {
                    let inner_schema = *(*schema).parameters;
                    let inner_width = (*inner_schema).width;
                    let inner_abs = shm::shmalloc(inner_width)?;
                    ptr::write_bytes(inner_abs, 0, inner_width);
                    let inner_expr = *tv;
                    morloc_eval_r(inner_expr, inner_abs, inner_width, bndvars)?;
                    *slot = shm::abs2rel(inner_abs)?;
                }
            } else if stype == crate::schema::SerialType::Int as u32 {
                // Variable-width integer: parse decimal string into inline BigInt
                let s = std::mem::ManuallyDrop::into_inner(ptr::read(&(*data).data.lit_val)).s;
                let decimal = if s.is_null() { "0" } else {
                    std::ffi::CStr::from_ptr(s).to_str().unwrap_or("0")
                };
                let limbs = decimal_to_limbs(decimal)?;
                let nlimbs = limbs.len();
                let fields = dest as *mut i64;
                if nlimbs <= 1 {
                    // Inline: [size=nlimbs, value]
                    *fields = nlimbs as i64;
                    *fields.add(1) = if nlimbs == 1 { limbs[0] as i64 } else { 0 };
                } else {
                    // Overflow: [size=nlimbs, relptr to limb array]
                    let abs = shm::shmemcpy(
                        limbs.as_ptr() as *const u8,
                        nlimbs * std::mem::size_of::<u64>(),
                    )?;
                    *(dest as *mut usize) = nlimbs;
                    *(dest.add(std::mem::size_of::<usize>()) as *mut RelPtr) = shm::abs2rel(abs)?;
                }
            } else {
                // All primitives: just copy width bytes from the union
                ptr::copy_nonoverlapping(
                    &(*data).data as *const DataUnion as *const u8,
                    dest,
                    width,
                );
            }
        }

        MorlocExpressionType::App => {
            let app = (*expr).expr.app_expr;
            let nargs = (*app).nargs;

            // Evaluate all arguments
            let mut arg_results: Vec<AbsPtr> = Vec::with_capacity(nargs);
            for i in 0..nargs {
                let r = morloc_eval_r(*(*app).args.add(i), ptr::null_mut(), 0, bndvars)?;
                arg_results.push(r);
            }

            match (*app).atype {
                MorlocAppExpressionType::Pattern => {
                    let pattern = (*app).function.pattern;
                    match (*pattern).ptype {
                        MorlocPatternType::BracketIndex => {
                            if nargs != 2 {
                                return Err(MorlocError::Other(
                                    "BracketIndex expects 2 args (index, receiver)".into(),
                                ));
                            }
                            apply_bracket_index(
                                dest, schema,
                                arg_results[0], (*(*(*app).args)).schema,
                                arg_results[1], (*(*(*app).args.add(1))).schema,
                            )?;
                        }
                        MorlocPatternType::BracketSlice => {
                            if nargs != 4 {
                                return Err(MorlocError::Other(
                                    "BracketSlice expects 4 args (start, stop, step, receiver)".into(),
                                ));
                            }
                            apply_bracket_slice(
                                dest, width, schema,
                                arg_results[0], (*(*(*app).args)).schema,
                                arg_results[1], (*(*(*app).args.add(1))).schema,
                                arg_results[2], (*(*(*app).args.add(2))).schema,
                                arg_results[3], (*(*(*app).args.add(3))).schema,
                            )?;
                        }
                        _ => {
                            // PatternStruct dispatch. The unified
                            // Selector can carry bracket steps (chain
                            // mode); 'count_bracket_arity' counts how
                            // many runtime args the selector will
                            // consume. With nargs == 1 + bracket_arity
                            // (and arity > 0), the args are
                            // [bracket_bounds..., receiver]; with
                            // nargs == 1 and arity == 0, it's a plain
                            // getter; with nargs > 1 and arity == 0,
                            // it's a setter.
                            let bracket_arity = count_bracket_arity(pattern);
                            if nargs == 1 + bracket_arity && bracket_arity > 0 {
                                // Unified bracket-in-Selector getter.
                                let receiver_ix = nargs - 1;
                                let receiver = arg_results[receiver_ix];
                                let receiver_schema = (*(*(*app).args.add(receiver_ix))).schema;
                                let bracket_args_vec: Vec<AbsPtr> =
                                    arg_results[..bracket_arity].to_vec();
                                let bracket_schemas_vec: Vec<*mut CSchema> = (0..bracket_arity)
                                    .map(|i| (*(*(*app).args.add(i))).schema)
                                    .collect();
                                let mut return_index: usize = 0;
                                let mut bracket_cursor: usize = 0;
                                let multi_terminal =
                                    count_pattern_terminals(pattern) > 1;
                                apply_getter(
                                    dest, &mut return_index, schema, multi_terminal,
                                    pattern,
                                    receiver_schema, receiver,
                                    &bracket_args_vec,
                                    &bracket_schemas_vec,
                                    &mut bracket_cursor,
                                )?;
                            } else if nargs == 1 {
                                let mut return_index: usize = 0;
                                let mut bracket_cursor: usize = 0;
                                let multi_terminal =
                                    count_pattern_terminals(pattern) > 1;
                                apply_getter(
                                    dest, &mut return_index, schema, multi_terminal,
                                    pattern,
                                    (*(*(*app).args)).schema,
                                    arg_results[0],
                                    &[], &[], &mut bracket_cursor,
                                )?;
                            } else if nargs > 1 {
                                // Setter: first arg is the value, rest are set values
                                let mut set_schemas: Vec<*mut CSchema> = Vec::with_capacity(nargs - 1);
                                for i in 1..nargs {
                                    set_schemas.push((*(*(*app).args.add(i))).schema);
                                }
                                apply_setter_copy(
                                    dest, schema, pattern,
                                    (*(*(*app).args)).schema, arg_results[0],
                                )?;
                                let mut set_idx: usize = 0;
                                apply_setter_set(
                                    dest, schema, pattern,
                                    (*(*(*app).args)).schema, arg_results[0],
                                    set_schemas.as_mut_ptr(), arg_results[1..].as_ptr() as *mut AbsPtr,
                                    &mut set_idx,
                                )?;
                            } else {
                                return Err(MorlocError::Other("No arguments provided to pattern".into()));
                            }
                        }
                    }
                }

                MorlocAppExpressionType::Lambda => {
                    let lam = (*app).function.lambda;
                    // Bind arguments
                    for i in 0..nargs {
                        let var = CStr::from_ptr(*(*lam).args.add(i)).to_str().unwrap_or("");
                        bndvars.insert(var, arg_results[i]);
                    }
                    morloc_eval_r((*lam).body, dest, width, bndvars)?;
                    // Clean up bindings
                    for i in 0..nargs {
                        let var = CStr::from_ptr(*(*lam).args.add(i)).to_str().unwrap_or("");
                        bndvars.remove(var);
                    }
                }

                MorlocAppExpressionType::Format => {
                    // Literal pieces are length-aware MorlocStrings so
                    // interior NULs survive. Previously this branch used
                    // libc::strlen, which truncated literals like "a\0b".
                    let strings = (*app).function.fmt;
                    let mut result_size: usize = 0;
                    for i in 0..=nargs {
                        let ms = *strings.add(i);
                        if !ms.is_null() {
                            result_size += (*ms).size;
                        }
                    }
                    for i in 0..nargs {
                        let arr = &*(arg_results[i] as *const shm::Array);
                        result_size += arr.size;
                    }

                    let new_string = shm::shmalloc(result_size)?;
                    let result_array = &mut *(dest as *mut shm::Array);
                    result_array.size = result_size;
                    result_array.data = shm::abs2rel(new_string)?;

                    let mut cursor = new_string;
                    for i in 0..=nargs {
                        let ms = *strings.add(i);
                        if !ms.is_null() && (*ms).size > 0 {
                            ptr::copy_nonoverlapping(
                                (*ms).data as *const u8,
                                cursor,
                                (*ms).size,
                            );
                            cursor = cursor.add((*ms).size);
                        }
                        if i < nargs {
                            let arr = &*(arg_results[i] as *const shm::Array);
                            if arr.size > 0 {
                                let arr_data = shm::rel2abs(arr.data)?;
                                ptr::copy_nonoverlapping(arr_data, cursor, arr.size);
                                cursor = cursor.add(arr.size);
                            }
                        }
                    }
                }
            }
        }

        MorlocExpressionType::Bnd => {
            let varname = CStr::from_ptr((*expr).expr.bnd_expr).to_str().unwrap_or("");
            let bnd_value = bndvars.get(varname).copied()
                .ok_or_else(|| MorlocError::Other(format!("Unbound variable {}", varname)))?;
            ptr::copy_nonoverlapping(bnd_value, dest, (*schema).width);
        }

        MorlocExpressionType::Show => {
            // Serialize child to JSON string
            let child = (*expr).expr.unary_expr;
            let child_schema = (*child).schema;
            let child_result = morloc_eval_r(child, ptr::null_mut(), 0, bndvars)?;

            extern "C" {
                fn voidstar_to_json_string(data: *const c_void, schema: *const CSchema, errmsg: *mut *mut c_char) -> *mut c_char;
            }
            let mut err: *mut c_char = ptr::null_mut();
            let json = voidstar_to_json_string(child_result as *const c_void, child_schema, &mut err);
            if !err.is_null() {
                let msg = CStr::from_ptr(err).to_string_lossy().into_owned();
                libc::free(err as *mut c_void);
                return Err(MorlocError::Other(msg));
            }

            let json_len = libc::strlen(json);
            let str_relptr: RelPtr = if json_len > 0 {
                let abs = shm::shmemcpy(json as *const u8, json_len)?;
                libc::free(json as *mut c_void);
                shm::abs2rel(abs)?
            } else {
                libc::free(json as *mut c_void);
                -1isize as RelPtr
            };
            let arr = shm::Array { size: json_len, data: str_relptr };
            ptr::copy_nonoverlapping(&arr as *const shm::Array as *const u8, dest, width);
        }

        MorlocExpressionType::Read => {
            // Deserialize JSON string to typed data, return optional.
            //
            // Optional is now a single relptr slot (RELNULL = absent).
            // Allocate the inner T's body in SHM, parse the JSON into
            // it, and write the relptr into the slot. Empty string or
            // parse failure → RELNULL.
            let child = (*expr).expr.unary_expr;
            let child_result = morloc_eval_r(child, ptr::null_mut(), 0, bndvars)?;
            let str_arr = &*(child_result as *const shm::Array);

            let opt_slot = dest as *mut shm::RelPtr;
            let inner_schema = *(*schema).parameters;
            let inner_width = (*inner_schema).width;

            if str_arr.size > 0 {
                let str_abs = shm::rel2abs(str_arr.data)?;
                let json_str = libc::malloc(str_arr.size + 1) as *mut c_char;
                if json_str.is_null() {
                    return Err(MorlocError::Other("Failed to allocate for @read".into()));
                }
                ptr::copy_nonoverlapping(str_abs, json_str as *mut u8, str_arr.size);
                *json_str.add(str_arr.size) = 0;

                extern "C" {
                    fn read_json_with_schema(dest: *mut u8, json: *mut c_char, schema: *const CSchema, errmsg: *mut *mut c_char) -> *mut u8;
                }

                let inner_abs = shm::shmalloc(inner_width)?;
                ptr::write_bytes(inner_abs, 0, inner_width);
                let mut parse_err: *mut c_char = ptr::null_mut();
                let parsed = read_json_with_schema(inner_abs, json_str, inner_schema, &mut parse_err);
                libc::free(json_str as *mut c_void);

                if !parse_err.is_null() || parsed.is_null() {
                    libc::free(parse_err as *mut c_void);
                    let _ = shm::shfree(inner_abs);
                    *opt_slot = shm::RELNULL;
                } else {
                    *opt_slot = shm::abs2rel(inner_abs)?;
                }
            } else {
                *opt_slot = shm::RELNULL;
            }
        }

        MorlocExpressionType::Hash => {
            // Hash child data and return hex string
            let child = (*expr).expr.unary_expr;
            let child_schema = (*child).schema;
            let child_result = morloc_eval_r(child, ptr::null_mut(), 0, bndvars)?;

            extern "C" {
                fn mlc_hash(data: *const c_void, schema: *const CSchema, errmsg: *mut *mut c_char) -> *mut c_char;
            }
            let mut err: *mut c_char = ptr::null_mut();
            let hex = mlc_hash(child_result as *const c_void, child_schema, &mut err);
            if !err.is_null() {
                let msg = CStr::from_ptr(err).to_string_lossy().into_owned();
                libc::free(err as *mut c_void);
                return Err(MorlocError::Other(msg));
            }

            let hex_len = libc::strlen(hex);
            let str_relptr: RelPtr = if hex_len > 0 {
                let abs = shm::shmemcpy(hex as *const u8, hex_len)?;
                libc::free(hex as *mut c_void);
                shm::abs2rel(abs)?
            } else {
                libc::free(hex as *mut c_void);
                -1isize as RelPtr
            };
            let arr = shm::Array { size: hex_len, data: str_relptr };
            ptr::copy_nonoverlapping(&arr as *const shm::Array as *const u8, dest, width);
        }

        MorlocExpressionType::Save => {
            // Save value to file at path
            let save = (*expr).expr.save_expr;
            let level_expr = (*save).level;
            let value_expr = (*save).value;
            let path_expr = (*save).path;
            let fmt = CStr::from_ptr((*save).format).to_str().unwrap_or("voidstar");

            let value_schema = (*value_expr).schema;
            // Evaluate the compression level. The level expression is
            // typechecked as Int; clamp into u8 with a 0-9 range check.
            let level_schema = (*level_expr).schema;
            let level_result = morloc_eval_r(level_expr, ptr::null_mut(), 0, bndvars)?;
            let level_i64 = read_int_as_i64(level_result, level_schema)?;
            if !(0..=9).contains(&level_i64) {
                return Err(MorlocError::Other(format!(
                    "@save compression level must be in 0..=9, got {level_i64}"
                )));
            }
            let level: u8 = level_i64 as u8;

            let value_result = morloc_eval_r(value_expr, ptr::null_mut(), 0, bndvars)?;
            let path_result = morloc_eval_r(path_expr, ptr::null_mut(), 0, bndvars)?;

            // Extract path string from voidstar Array
            let path_arr = &*(path_result as *const shm::Array);
            let path_abs = shm::rel2abs(path_arr.data)?;
            let path_cstr = libc::malloc(path_arr.size + 1) as *mut c_char;
            if path_cstr.is_null() {
                return Err(MorlocError::Other("Failed to allocate for @save path".into()));
            }
            ptr::copy_nonoverlapping(path_abs, path_cstr as *mut u8, path_arr.size);
            *path_cstr.add(path_arr.size) = 0;

            extern "C" {
                fn mlc_save(data: *const c_void, schema: *const CSchema, level: u8, path: *const c_char, errmsg: *mut *mut c_char) -> i32;
                fn mlc_save_json(data: *const c_void, schema: *const CSchema, level: u8, path: *const c_char, errmsg: *mut *mut c_char) -> i32;
                fn mlc_save_voidstar(data: *const c_void, schema: *const CSchema, level: u8, path: *const c_char, errmsg: *mut *mut c_char) -> i32;
            }
            let mut err: *mut c_char = ptr::null_mut();
            let rc = match fmt {
                "json" => mlc_save_json(value_result as *const c_void, value_schema, level, path_cstr, &mut err),
                "msgpack" => mlc_save(value_result as *const c_void, value_schema, level, path_cstr, &mut err),
                _ => mlc_save_voidstar(value_result as *const c_void, value_schema, level, path_cstr, &mut err),
            };
            libc::free(path_cstr as *mut c_void);
            if rc != 0 && !err.is_null() {
                let msg = CStr::from_ptr(err).to_string_lossy().into_owned();
                libc::free(err as *mut c_void);
                return Err(MorlocError::Other(msg));
            }
            // Return unit (zero-fill dest)
            ptr::write_bytes(dest, 0, width);
        }

        MorlocExpressionType::Load => {
            // Load data from file, return optional.
            //
            // Optional is now a single relptr slot. On a successful
            // load we allocate the inner T's body in SHM, deep-copy
            // the loaded data into it, and store the relptr in the
            // slot. On failure (file missing, parse error, etc.) the
            // slot gets RELNULL.
            let child = (*expr).expr.unary_expr;
            let child_result = morloc_eval_r(child, ptr::null_mut(), 0, bndvars)?;

            // Extract path string from voidstar Array
            let path_arr = &*(child_result as *const shm::Array);
            let path_abs = shm::rel2abs(path_arr.data)?;
            let path_cstr = libc::malloc(path_arr.size + 1) as *mut c_char;
            if path_cstr.is_null() {
                return Err(MorlocError::Other("Failed to allocate for @load path".into()));
            }
            ptr::copy_nonoverlapping(path_abs, path_cstr as *mut u8, path_arr.size);
            *path_cstr.add(path_arr.size) = 0;

            extern "C" {
                fn mlc_load(path: *const c_char, schema: *const CSchema, errmsg: *mut *mut c_char) -> *mut c_void;
            }
            let opt_slot = dest as *mut shm::RelPtr;
            let inner_schema = *(*schema).parameters;
            let inner_width = (*inner_schema).width;

            let mut err: *mut c_char = ptr::null_mut();
            let loaded = mlc_load(path_cstr, inner_schema, &mut err);
            libc::free(path_cstr as *mut c_void);

            if loaded.is_null() {
                if !err.is_null() {
                    libc::free(err as *mut c_void);
                }
                *opt_slot = shm::RELNULL;
            } else {
                // mlc_load returns SHM but the layout depends on the file
                // format: msgpack and voidstar pack the wrapper and nested
                // data into a single block (relptrs reference addresses
                // inside the block), JSON returns a multi-block tree. A
                // straight bit-copy of the wrapper would alias the source
                // block, and libc::free on SHM is undefined. Deep-copy
                // into a fresh inner allocation so the result matches what
                // the rest of the evaluator produces, then shfree the
                // source block as one allocation.
                let inner_rs = crate::cschema::CSchema::to_rust(inner_schema);
                let inner_abs = shm::shmalloc(inner_width)?;
                ptr::write_bytes(inner_abs, 0, inner_width);
                let copy_result = crate::voidstar::deep_copy(
                    loaded as *const u8,
                    inner_abs,
                    &inner_rs,
                );
                let _ = shm::shfree(loaded as shm::AbsPtr);
                match copy_result {
                    Ok(_) => {
                        *opt_slot = shm::abs2rel(inner_abs)?;
                    }
                    Err(e) => {
                        let _ = shm::shfree(inner_abs);
                        return Err(e);
                    }
                }
            }
        }

        MorlocExpressionType::Map => {
            // Pure-morloc list map. Apply `func` (a one-parameter lambda)
            // to each element of `list`, producing a fresh Array. The
            // output element stride is fixed by the output schema's
            // element width: variable-width content inside each element
            // (Array data buffers, Optional inner T, BigInt limbs) lives
            // out-of-line via relptrs allocated by the per-element
            // recursive eval, mirroring the array-literal path above.
            let me = (*expr).expr.map_expr;
            let func_expr = (*me).func;
            let list_expr = (*me).list;

            // Output element schema/width from the surrounding Map node's schema.
            let b_schema = *(*schema).parameters;
            let b_width = (*b_schema).width;

            // Input element width from the list expression's own schema.
            let list_csch = (*list_expr).schema;
            let a_schema = *(*list_csch).parameters;
            let a_width = (*a_schema).width;

            // Evaluate the list. The result is a fresh AbsPtr to an Array header.
            let in_ptr = morloc_eval_r(list_expr, ptr::null_mut(), 0, bndvars)?;
            let in_arr = &*(in_ptr as *const shm::Array);
            let n = in_arr.size;

            let out_relptr: RelPtr = if n == 0 {
                shm::RELNULL
            } else {
                let in_data = shm::rel2abs(in_arr.data)?;
                let out_data = shm::shcalloc(n, b_width)?;

                if (*func_expr).etype != MorlocExpressionType::Lam {
                    return Err(MorlocError::Other(
                        "Map func must be a lambda".into(),
                    ));
                }
                let lam = (*func_expr).expr.lam_expr;
                if (*lam).nargs != 1 {
                    return Err(MorlocError::Other(
                        "Map lambda must take exactly one argument".into(),
                    ));
                }
                let param = CStr::from_ptr(*(*lam).args).to_str().unwrap_or("");
                let body = (*lam).body;

                for i in 0..n {
                    let in_elem = in_data.add(i * a_width);
                    let out_elem = out_data.add(i * b_width);
                    bndvars.insert(param, in_elem);
                    morloc_eval_r(body, out_elem, b_width, bndvars)?;
                    bndvars.remove(param);
                }
                shm::abs2rel(out_data)?
            };

            let header = shm::Array { size: n, data: out_relptr };
            ptr::copy_nonoverlapping(
                &header as *const shm::Array as *const u8,
                dest,
                width,
            );
        }

        MorlocExpressionType::Open => {
            // Dispatch on handle kind. IFile's wire form is the path
            // laid into a tagged stream-handle field (TAG_PATH); the
            // receiving pool reopens locally. IStream needs a stable
            // cursor and OStream cannot be reopened (O_EXCL), so for
            // those the nexus opens once and stores the real i64
            // handle in dest.
            let open = (*expr).expr.open_expr;
            let kind = (*open).kind;
            let path_expr = (*open).path;
            let path_result = morloc_eval_r(path_expr, ptr::null_mut(), 0, bndvars)?;
            let path_cstr = path_voidstar_to_cstr(path_result, "@open")?;

            if kind == 0 {
                // IFile: write the path into a TAG_PATH stream-handle
                // field. Lazy validation: the first downstream read
                // goes through mlc_read_stream_field which opens and
                // surfaces any ENOENT / format error then.
                use morloc_runtime_types::stream_handle as sh;
                let path_len = libc::strlen(path_cstr);
                let payload = if path_len == 0 {
                    sh::RELNULL_PAYLOAD
                } else {
                    let block = shm::shmalloc(sh::path_suballoc_size(path_len))?;
                    sh::write_path_suballoc(
                        block,
                        std::slice::from_raw_parts(path_cstr as *const u8, path_len),
                    );
                    shm::abs2rel(block)? as u64
                };
                sh::write_field(dest, sh::TAG_PATH, payload);
                libc::free(path_cstr as *mut c_void);
            } else {
                // IStream / OStream: actually open via the runtime so
                // the cursor / fd is stable across subsequent ops in
                // this nexus scope. The eval_arena tracks and closes
                // the handle when the scope ends. The result lands in
                // dest as a TAG_HANDLE stream-handle field so downstream
                // reads go through mlc_read_stream_field uniformly with
                // the IFile TAG_PATH case above.
                extern "C" {
                    fn mlc_open(path: *const c_char, kind: u8, errmsg: *mut *mut c_char) -> i64;
                }
                use morloc_runtime_types::stream_handle as sh;
                let mut err: *mut c_char = ptr::null_mut();
                let handle = mlc_open(path_cstr, kind, &mut err);
                libc::free(path_cstr as *mut c_void);
                if handle < 0 {
                    let msg = if !err.is_null() {
                        let m = CStr::from_ptr(err).to_string_lossy().into_owned();
                        libc::free(err as *mut c_void);
                        m
                    } else {
                        format!("mlc_open(kind={}) returned -1 without errmsg", kind)
                    };
                    return Err(MorlocError::Other(msg));
                }
                sh::write_field(dest, sh::TAG_HANDLE, handle as u64);
            }
        }

        MorlocExpressionType::Close => {
            extern "C" {
                fn mlc_close(handle: i64, errmsg: *mut *mut c_char) -> i32;
            }
            let handle_expr = (*expr).expr.unary_expr;
            let handle_schema = (*handle_expr).schema;
            let handle_ptr = morloc_eval_r(handle_expr, ptr::null_mut(), 0, bndvars)?;
            let handle_i = read_int_as_i64(handle_ptr, handle_schema)?;
            let mut err: *mut c_char = ptr::null_mut();
            let rc = mlc_close(handle_i, &mut err);
            if rc != 0 {
                let msg = if !err.is_null() {
                    let s = CStr::from_ptr(err).to_string_lossy().into_owned();
                    libc::free(err as *mut c_void);
                    s
                } else {
                    "mlc_close returned non-zero with no error message".to_string()
                };
                return Err(MorlocError::Other(msg));
            }
            // Return unit (zero-fill dest).
            ptr::write_bytes(dest, 0, width);
        }

        MorlocExpressionType::FSchema => {
            extern "C" {
                fn mlc_fschema(path: *const c_char, errmsg: *mut *mut c_char) -> *mut c_char;
            }
            let path_expr = (*expr).expr.unary_expr;
            let path_result = morloc_eval_r(path_expr, ptr::null_mut(), 0, bndvars)?;
            let path_cstr = path_voidstar_to_cstr(path_result, "@fschema")?;
            let mut err: *mut c_char = ptr::null_mut();
            let s = mlc_fschema(path_cstr, &mut err);
            libc::free(path_cstr as *mut c_void);
            if s.is_null() {
                let msg = if !err.is_null() {
                    let m = CStr::from_ptr(err).to_string_lossy().into_owned();
                    libc::free(err as *mut c_void);
                    m
                } else {
                    "mlc_fschema returned NULL with no error message".to_string()
                };
                return Err(MorlocError::Other(msg));
            }
            // Materialise the returned string into a voidstar Str
            // (Array of u8) in SHM, then write the Array header into dest.
            let len = libc::strlen(s);
            let data_abs = shm::shmemcpy(s as *const u8, len)?;
            libc::free(s as *mut c_void);
            let header = shm::Array { size: len, data: shm::abs2rel(data_abs)? };
            ptr::copy_nonoverlapping(
                &header as *const shm::Array as *const u8,
                dest,
                std::mem::size_of::<shm::Array>(),
            );
        }

        MorlocExpressionType::FLength => {
            extern "C" {
                fn mlc_ifile_length(handle: i64, errmsg: *mut *mut c_char) -> i64;
            }
            let handle_expr = (*expr).expr.unary_expr;
            let handle_schema = (*handle_expr).schema;
            let handle_ptr = morloc_eval_r(handle_expr, ptr::null_mut(), 0, bndvars)?;
            let handle_i = read_int_as_i64(handle_ptr, handle_schema)?;
            let mut err: *mut c_char = ptr::null_mut();
            let n = mlc_ifile_length(handle_i, &mut err);
            if n < 0 {
                let msg = if !err.is_null() {
                    let m = CStr::from_ptr(err).to_string_lossy().into_owned();
                    libc::free(err as *mut c_void);
                    m
                } else {
                    "mlc_ifile_length returned -1".to_string()
                };
                return Err(MorlocError::Other(msg));
            }
            write_int_to_dest(dest, schema, n)?;
        }

        MorlocExpressionType::Next => {
            extern "C" {
                fn mlc_next(handle: i64, errmsg: *mut *mut c_char) -> *mut c_void;
            }
            let handle_expr = (*expr).expr.unary_expr;
            let handle_schema = (*handle_expr).schema;
            let handle_ptr = morloc_eval_r(handle_expr, ptr::null_mut(), 0, bndvars)?;
            let handle_i = read_int_as_i64(handle_ptr, handle_schema)?;
            let mut err: *mut c_char = ptr::null_mut();
            let voidstar = mlc_next(handle_i, &mut err);
            if voidstar.is_null() {
                let msg = if !err.is_null() {
                    let m = CStr::from_ptr(err).to_string_lossy().into_owned();
                    libc::free(err as *mut c_void);
                    m
                } else {
                    "mlc_next returned NULL".to_string()
                };
                return Err(MorlocError::Other(msg));
            }
            let result_rs = crate::cschema::CSchema::to_rust(schema);
            crate::voidstar::deep_copy(voidstar as *const u8, dest, &result_rs)?;
            let _ = shm::shfree(voidstar as shm::AbsPtr);
        }

        MorlocExpressionType::Stream => {
            extern "C" {
                fn mlc_stream(ifile_handle: i64, errmsg: *mut *mut c_char) -> i64;
            }
            use morloc_runtime_types::stream_handle as sh;
            let handle_expr = (*expr).expr.unary_expr;
            let handle_schema = (*handle_expr).schema;
            let handle_ptr = morloc_eval_r(handle_expr, ptr::null_mut(), 0, bndvars)?;
            let handle_i = read_int_as_i64(handle_ptr, handle_schema)?;
            let mut err: *mut c_char = ptr::null_mut();
            let new_h = mlc_stream(handle_i, &mut err);
            if new_h < 0 {
                let msg = if !err.is_null() {
                    let m = CStr::from_ptr(err).to_string_lossy().into_owned();
                    libc::free(err as *mut c_void);
                    m
                } else {
                    "mlc_stream returned -1".to_string()
                };
                return Err(MorlocError::Other(msg));
            }
            sh::write_field(dest, sh::TAG_HANDLE, new_h as u64);
        }

        MorlocExpressionType::IFileWalk => {
            extern "C" {
                fn mlc_ifile_walk(
                    handle: i64,
                    path: *const c_char,
                    args_ptr: *const crate::intrinsics::IFileWalkArg,
                    n_args: u64,
                    errmsg: *mut *mut c_char,
                ) -> *mut c_void;
            }
            let w = (*expr).expr.ifile_walk_expr;
            let handle_expr = (*w).handle;
            let path_cstr = (*w).path;
            let n_args = (*w).n_args as usize;

            let handle_schema = (*handle_expr).schema;
            let handle_ptr = morloc_eval_r(handle_expr, ptr::null_mut(), 0, bndvars)?;
            let handle_i = read_int_as_i64(handle_ptr, handle_schema)?;

            // Evaluate each runtime arg into an ?Int64 -> presence flag +
            // value pair. The C ABI expects a packed array of IFileWalkArg.
            let args_storage: Vec<crate::intrinsics::IFileWalkArg> = if n_args == 0 {
                Vec::new()
            } else {
                let mut v = Vec::with_capacity(n_args);
                let args_ptr = (*w).args;
                for i in 0..n_args {
                    let a_expr = *args_ptr.add(i);
                    let a_schema = (*a_expr).schema;
                    let a_ptr = morloc_eval_r(a_expr, ptr::null_mut(), 0, bndvars)?;
                    let opt = read_optional_int(a_ptr, a_schema)?;
                    let (has, value) = match opt {
                        Some(x) => (1u8, x),
                        None => (0u8, 0i64),
                    };
                    v.push(crate::intrinsics::IFileWalkArg {
                        has,
                        _pad: [0; 7],
                        value,
                    });
                }
                v
            };

            let mut err: *mut c_char = ptr::null_mut();
            let voidstar = mlc_ifile_walk(
                handle_i,
                path_cstr,
                if args_storage.is_empty() { ptr::null() } else { args_storage.as_ptr() },
                n_args as u64,
                &mut err,
            );
            if voidstar.is_null() {
                let msg = if !err.is_null() {
                    let m = CStr::from_ptr(err).to_string_lossy().into_owned();
                    libc::free(err as *mut c_void);
                    m
                } else {
                    "mlc_ifile_walk returned NULL".to_string()
                };
                return Err(MorlocError::Other(msg));
            }
            let result_rs = crate::cschema::CSchema::to_rust(schema);
            crate::voidstar::deep_copy(voidstar as *const u8, dest, &result_rs)?;
            let _ = shm::shfree(voidstar as shm::AbsPtr);
        }

        MorlocExpressionType::OpenOStream => {
            // Typed OStream open: schema (parsed CSchema sitting in
            // expr.schema) + path expression on open_expr. Serialise
            // the schema back to its canonical string for the runtime
            // entry, then store the returned handle in dest as a
            // TAG_HANDLE stream-handle field so downstream reads go
            // through mlc_read_stream_field uniformly with the IFile
            // TAG_PATH / IStream TAG_HANDLE cases.
            extern "C" {
                fn mlc_open_ostream(
                    schema_str: *const c_char,
                    path: *const c_char,
                    errmsg: *mut *mut c_char,
                ) -> i64;
                fn schema_to_string(schema: *const crate::cschema::CSchema) -> *mut c_char;
            }
            use morloc_runtime_types::stream_handle as sh;
            let open = (*expr).expr.open_expr;
            let path_expr = (*open).path;
            let path_result = morloc_eval_r(path_expr, ptr::null_mut(), 0, bndvars)?;
            let path_cstr = path_voidstar_to_cstr(path_result, "@open OStream")?;
            let schema_cstr = schema_to_string(schema);
            if schema_cstr.is_null() {
                libc::free(path_cstr as *mut c_void);
                return Err(MorlocError::Other(
                    "@open OStream: schema_to_string returned NULL".into(),
                ));
            }
            let mut err: *mut c_char = ptr::null_mut();
            let handle = mlc_open_ostream(schema_cstr, path_cstr, &mut err);
            libc::free(path_cstr as *mut c_void);
            libc::free(schema_cstr as *mut c_void);
            if handle < 0 {
                let msg = if !err.is_null() {
                    let m = CStr::from_ptr(err).to_string_lossy().into_owned();
                    libc::free(err as *mut c_void);
                    m
                } else {
                    "mlc_open_ostream returned -1 without errmsg".to_string()
                };
                return Err(MorlocError::Other(msg));
            }
            sh::write_field(dest, sh::TAG_HANDLE, handle as u64);
        }

        MorlocExpressionType::Write => {
            // @write: layout via ifile_walk_expr -- handle in .handle,
            // args[0] = level, args[1] = value (a `[a]` voidstar).
            extern "C" {
                fn mlc_write(
                    level: u8, handle: i64,
                    payload_voidstar: *const c_void,
                    errmsg: *mut *mut c_char,
                ) -> i32;
            }
            let w = (*expr).expr.ifile_walk_expr;
            let handle_expr = (*w).handle;
            let handle_schema = (*handle_expr).schema;
            let handle_ptr = morloc_eval_r(handle_expr, ptr::null_mut(), 0, bndvars)?;
            let handle_i = read_int_as_i64(handle_ptr, handle_schema)?;
            if (*w).n_args < 2 || (*w).args.is_null() {
                return Err(MorlocError::Other(
                    "@write: missing level or value args (manifest corruption)".into(),
                ));
            }
            let level_expr = *(*w).args.add(0);
            let value_expr = *(*w).args.add(1);
            let level_schema = (*level_expr).schema;
            let level_ptr = morloc_eval_r(level_expr, ptr::null_mut(), 0, bndvars)?;
            let level_i = read_int_as_i64(level_ptr, level_schema)?;
            let value_voidstar = morloc_eval_r(value_expr, ptr::null_mut(), 0, bndvars)?;
            let mut err: *mut c_char = ptr::null_mut();
            let rc = mlc_write(level_i as u8, handle_i, value_voidstar as *const c_void, &mut err);
            if rc != 0 {
                let msg = if !err.is_null() {
                    let m = CStr::from_ptr(err).to_string_lossy().into_owned();
                    libc::free(err as *mut c_void);
                    m
                } else {
                    "mlc_write returned non-zero without errmsg".to_string()
                };
                return Err(MorlocError::Other(msg));
            }
            // Return unit (zero-fill dest).
            ptr::write_bytes(dest, 0, width);
        }

        MorlocExpressionType::Append => {
            extern "C" {
                fn mlc_append(
                    schema_str: *const c_char,
                    path: *const c_char,
                    errmsg: *mut *mut c_char,
                ) -> i64;
                fn schema_to_string(schema: *const crate::cschema::CSchema) -> *mut c_char;
            }
            let open = (*expr).expr.open_expr;
            let path_expr = (*open).path;
            let path_result = morloc_eval_r(path_expr, ptr::null_mut(), 0, bndvars)?;
            let path_cstr = path_voidstar_to_cstr(path_result, "@append")?;
            let schema_cstr = schema_to_string(schema);
            if schema_cstr.is_null() {
                libc::free(path_cstr as *mut c_void);
                return Err(MorlocError::Other(
                    "@append: schema_to_string returned NULL".into(),
                ));
            }
            let mut err: *mut c_char = ptr::null_mut();
            let handle = mlc_append(schema_cstr, path_cstr, &mut err);
            libc::free(path_cstr as *mut c_void);
            libc::free(schema_cstr as *mut c_void);
            if handle < 0 {
                let msg = if !err.is_null() {
                    let m = CStr::from_ptr(err).to_string_lossy().into_owned();
                    libc::free(err as *mut c_void);
                    m
                } else {
                    "mlc_append returned -1 without errmsg".to_string()
                };
                return Err(MorlocError::Other(msg));
            }
            use morloc_runtime_types::stream_handle as sh;
            sh::write_field(dest, sh::TAG_HANDLE, handle as u64);
        }

        MorlocExpressionType::Concat => {
            // @concat: ifile_walk_expr layout -- handle = paths
            // (a `[Str]` voidstar Array<Array<u8>>), args[0] = dest path.
            extern "C" {
                fn mlc_concat(
                    paths: *const *const c_char,
                    n_paths: usize,
                    dest: *const c_char,
                    errmsg: *mut *mut c_char,
                ) -> i32;
            }
            let w = (*expr).expr.ifile_walk_expr;
            let paths_expr = (*w).handle;
            if (*w).n_args < 1 || (*w).args.is_null() {
                return Err(MorlocError::Other(
                    "@concat: missing dest arg (manifest corruption)".into(),
                ));
            }
            let dest_expr = *(*w).args.add(0);
            let paths_result = morloc_eval_r(paths_expr, ptr::null_mut(), 0, bndvars)?;
            let dest_result = morloc_eval_r(dest_expr, ptr::null_mut(), 0, bndvars)?;
            let dest_cstr = path_voidstar_to_cstr(dest_result, "@concat dest")?;

            // Unpack the `[Str]` voidstar at paths_result. Each element
            // is an Array<u8> path. Build a Vec<CString> + Vec<*const c_char>.
            let paths_arr = &*(paths_result as *const shm::Array);
            let n = paths_arr.size;
            let mut owned: Vec<*mut c_char> = Vec::with_capacity(n);
            let mut raw: Vec<*const c_char> = Vec::with_capacity(n);
            let elem_data = if n > 0 {
                shm::rel2abs(paths_arr.data)?
            } else {
                ptr::null_mut()
            };
            for i in 0..n {
                let elem_ptr = (elem_data as *const u8)
                    .add(i * std::mem::size_of::<shm::Array>())
                    as *const shm::Array;
                let one = path_voidstar_to_cstr(
                    elem_ptr as shm::AbsPtr,
                    "@concat path element",
                )?;
                owned.push(one);
                raw.push(one);
            }

            let mut err: *mut c_char = ptr::null_mut();
            let rc = mlc_concat(
                if raw.is_empty() { ptr::null() } else { raw.as_ptr() },
                n,
                dest_cstr,
                &mut err,
            );
            for p in owned { libc::free(p as *mut c_void); }
            libc::free(dest_cstr as *mut c_void);
            if rc != 0 {
                let msg = if !err.is_null() {
                    let m = CStr::from_ptr(err).to_string_lossy().into_owned();
                    libc::free(err as *mut c_void);
                    m
                } else {
                    "mlc_concat returned non-zero without errmsg".to_string()
                };
                return Err(MorlocError::Other(msg));
            }
            // Return unit.
            ptr::write_bytes(dest, 0, width);
        }

        MorlocExpressionType::Flush => {
            extern "C" {
                fn mlc_flush(handle: i64, errmsg: *mut *mut c_char) -> i32;
            }
            let handle_expr = (*expr).expr.unary_expr;
            let handle_schema = (*handle_expr).schema;
            let handle_ptr = morloc_eval_r(handle_expr, ptr::null_mut(), 0, bndvars)?;
            let handle_i = read_int_as_i64(handle_ptr, handle_schema)?;
            let mut err: *mut c_char = ptr::null_mut();
            let rc = mlc_flush(handle_i, &mut err);
            if rc != 0 {
                let msg = if !err.is_null() {
                    let s = CStr::from_ptr(err).to_string_lossy().into_owned();
                    libc::free(err as *mut c_void);
                    s
                } else {
                    "mlc_flush returned non-zero with no error message".to_string()
                };
                return Err(MorlocError::Other(msg));
            }
            ptr::write_bytes(dest, 0, width);
        }

        other => {
            // Catch-all: the manifest carried an expression type that
            // this eval handler doesn't recognise. Include the
            // numeric tag and the variant name so the diagnostic
            // points at the missing case (typically: new JSON tag was
            // added on the codegen side, MorlocExpressionType variant
            // was added on the runtime side, but the eval_ffi match
            // arm wasn't wired -- this is the symptom).
            return Err(MorlocError::Other(format!(
                "eval_ffi: no handler for MorlocExpressionType::{:?} \
                 (numeric tag {}). The manifest references an \
                 expression kind this runtime build doesn't implement \
                 -- either the morloc compiler emitted a tag the \
                 runtime doesn't know, or the eval_ffi.rs match arm \
                 for this variant is missing.",
                other, other as u32,
            )));
        }
    }

    Ok(dest)
}

/// Extract a NUL-terminated path C-string from a voidstar Str
/// (an `Array<u8>` of UTF-8 bytes). Used by IFile-family eval cases
/// that pass paths through to libmorloc.so. The returned buffer is
/// libc-malloc'd and the caller is responsible for `libc::free`-ing it.
unsafe fn path_voidstar_to_cstr(
    path_voidstar: AbsPtr,
    intrinsic_name: &str,
) -> Result<*mut c_char, MorlocError> {
    let arr = &*(path_voidstar as *const shm::Array);
    let abs = shm::rel2abs(arr.data)?;
    let buf = libc::malloc(arr.size + 1) as *mut c_char;
    if buf.is_null() {
        return Err(MorlocError::Other(format!(
            "Failed to allocate path buffer for {}",
            intrinsic_name,
        )));
    }
    ptr::copy_nonoverlapping(abs, buf as *mut u8, arr.size);
    *buf.add(arr.size) = 0;
    Ok(buf)
}

// ── Public entry point ────���─────────────────────────────────��────────────────

#[no_mangle]
pub unsafe extern "C" fn morloc_eval(
    expr: *mut MorlocExpression,
    return_schema: *mut CSchema,
    arg_voidstar: *mut *mut u8,
    arg_schemas: *mut *mut CSchema,
    nargs: usize,
    errmsg: *mut *mut c_char,
) -> AbsPtr {
    clear_errmsg(errmsg);

    let mut bndvars: BndVars = HashMap::new();
    let new_expr: *mut MorlocExpression;
    let mut allocated_wrappers: Vec<*mut c_void> = Vec::new();

    let eval_expr = match (*expr).etype {
        MorlocExpressionType::Lam | MorlocExpressionType::Pat => {
            // Wrap CLI args as voidstar data expressions and apply
            let arg_exprs = libc::calloc(nargs, std::mem::size_of::<*mut MorlocExpression>()) as *mut *mut MorlocExpression;
            allocated_wrappers.push(arg_exprs as *mut c_void);

            for i in 0..nargs {
                let ae = libc::calloc(1, std::mem::size_of::<MorlocExpression>()) as *mut MorlocExpression;
                (*ae).etype = MorlocExpressionType::Dat;
                (*ae).schema = *arg_schemas.add(i);
                let ad = libc::calloc(1, std::mem::size_of::<MorlocData>()) as *mut MorlocData;
                (*ad).is_voidstar = true;
                (*ad).data.voidstar = *arg_voidstar.add(i) as *mut c_void;
                (*ae).expr.data_expr = ad;
                *arg_exprs.add(i) = ae;
                allocated_wrappers.push(ad as *mut c_void);
                allocated_wrappers.push(ae as *mut c_void);
            }

            let app = libc::calloc(1, std::mem::size_of::<MorlocAppExpression>()) as *mut MorlocAppExpression;
            (*app).atype = if (*expr).etype == MorlocExpressionType::Lam {
                (*app).function.lambda = (*expr).expr.lam_expr;
                MorlocAppExpressionType::Lambda
            } else {
                (*app).function.pattern = (*expr).expr.pattern_expr;
                MorlocAppExpressionType::Pattern
            };
            (*app).args = arg_exprs;
            (*app).nargs = nargs;
            allocated_wrappers.push(app as *mut c_void);

            new_expr = libc::calloc(1, std::mem::size_of::<MorlocExpression>()) as *mut MorlocExpression;
            (*new_expr).etype = MorlocExpressionType::App;
            (*new_expr).schema = return_schema;
            (*new_expr).expr.app_expr = app;
            allocated_wrappers.push(new_expr as *mut c_void);

            new_expr
        }
        _ => expr,
    };

    let result = morloc_eval_r(eval_expr, ptr::null_mut(), 0, &mut bndvars);

    // Free wrapper nodes
    for p in &allocated_wrappers {
        libc::free(*p);
    }

    match result {
        Ok(ptr) => ptr,
        Err(e) => {
            set_errmsg(errmsg, &e);
            ptr::null_mut()
        }
    }
}
