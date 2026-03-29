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

// ── Core evaluator ───────────────────────────────────────────────────────────

type BndVars<'a> = HashMap<&'a str, AbsPtr>;

/// Convert key-based pattern selectors to index-based using the schema's key names.
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

    if n_params > 1 {
        for i in 0..pat.size {
            let child_schema = *(*schema).parameters.add(i);
            convert_keys_to_indices(*pat.selectors.add(i), child_schema)?;
        }
    }

    if pat.ptype == MorlocPatternType::ByKey {
        let indices = libc::calloc(n_params, std::mem::size_of::<usize>()) as *mut usize;
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

    Ok(())
}

/// Extract fields from a voidstar value using a pattern, copying them into dest.
///
/// # Safety
/// All pointer arguments must be valid and point to correctly-typed C structures.
/// `value` must point to voidstar data matching `value_schema`.
unsafe fn apply_getter(
    dest: AbsPtr,
    return_index: &mut usize,
    return_schema: *const CSchema,
    pattern: *mut MorlocPattern,
    value_schema: *const CSchema,
    value: AbsPtr,
) -> Result<AbsPtr, MorlocError> {
    let pat = &*pattern;

    match pat.ptype {
        MorlocPatternType::ByIndex => {
            for i in 0..pat.size {
                let idx = *pat.fields.indices.add(i);
                apply_getter(
                    dest, return_index, return_schema,
                    *pat.selectors.add(i),
                    *(*value_schema).parameters.add(idx),
                    value.add(*(*value_schema).offsets.add(idx)),
                )?;
            }
        }
        MorlocPatternType::ByKey => {
            convert_keys_to_indices(pattern, value_schema)?;
            return apply_getter(dest, return_index, return_schema, pattern, value_schema, value);
        }
        MorlocPatternType::End => {
            let (element_dest, element_width) = if (*return_schema).size > 1 {
                (dest.add(*(*return_schema).offsets.add(*return_index)),
                 (*(*(*return_schema).parameters.add(*return_index))).width)
            } else {
                (dest, (*return_schema).width)
            };
            *return_index += 1;
            ptr::copy_nonoverlapping(value, element_dest, element_width);
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
    }
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
                // String: allocate in SHM
                let s = std::mem::ManuallyDrop::into_inner(ptr::read(&(*data).data.lit_val)).s;
                let str_size = if s.is_null() { 0 } else { libc::strlen(s) };
                let str_relptr: RelPtr = if str_size > 0 {
                    let abs = shm::shmemcpy(s as *const u8, str_size)?;
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
                    if nargs == 1 {
                        let mut return_index: usize = 0;
                        apply_getter(
                            dest, &mut return_index, schema,
                            (*app).function.pattern,
                            (*(*(*app).args)).schema,
                            arg_results[0],
                        )?;
                    } else if nargs > 1 {
                        // Setter: first arg is the value, rest are set values
                        let mut set_schemas: Vec<*mut CSchema> = Vec::with_capacity(nargs - 1);
                        for i in 1..nargs {
                            set_schemas.push((*(*(*app).args.add(i))).schema);
                        }
                        apply_setter_copy(
                            dest, schema, (*app).function.pattern,
                            (*(*(*app).args)).schema, arg_results[0],
                        )?;
                        let mut set_idx: usize = 0;
                        apply_setter_set(
                            dest, schema, (*app).function.pattern,
                            (*(*(*app).args)).schema, arg_results[0],
                            set_schemas.as_mut_ptr(), arg_results[1..].as_ptr() as *mut AbsPtr,
                            &mut set_idx,
                        )?;
                    } else {
                        return Err(MorlocError::Other("No arguments provided to pattern".into()));
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
                    let strings = (*app).function.fmt;
                    let mut result_size: usize = 0;
                    let mut string_lengths: Vec<usize> = Vec::with_capacity(nargs + 1);

                    for i in 0..=nargs {
                        let len = libc::strlen(*strings.add(i));
                        string_lengths.push(len);
                        result_size += len;
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
                        ptr::copy_nonoverlapping(*strings.add(i) as *const u8, cursor, string_lengths[i]);
                        cursor = cursor.add(string_lengths[i]);
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
            // Deserialize JSON string to typed data, return optional
            let child = (*expr).expr.unary_expr;
            let child_result = morloc_eval_r(child, ptr::null_mut(), 0, bndvars)?;
            let str_arr = &*(child_result as *const shm::Array);

            let opt_dest = dest;
            let inner_schema = *(*schema).parameters;

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
                let inner_offset = *(*schema).offsets;
                let mut parse_err: *mut c_char = ptr::null_mut();
                let parsed = read_json_with_schema(opt_dest.add(inner_offset), json_str, inner_schema, &mut parse_err);
                libc::free(json_str as *mut c_void);
                if !parse_err.is_null() {
                    libc::free(parse_err as *mut c_void);
                    *opt_dest = 0; // None
                } else {
                    *opt_dest = if parsed.is_null() { 0 } else { 1 };
                }
            } else {
                *opt_dest = 0; // None
            }
        }

        _ => {
            return Err(MorlocError::Other("Illegal top expression".into()));
        }
    }

    Ok(dest)
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
