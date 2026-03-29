//! C ABI wrappers for libmorloc.so
//!
//! These `extern "C"` functions match the signatures in morloc.h.
//! Internally they call the Rust implementations and convert between
//! Rust Result<T,E> and the C ERRMSG convention (char** last arg).

use std::ffi::{c_char, c_void, CStr, CString};
use std::ptr;

use crate::error::{clear_errmsg, set_errmsg, MorlocError};
use crate::schema::{self};
use crate::shm::{self, AbsPtr, BlockHeader, RelPtr, ShmHeader, VolPtr};
pub use crate::cschema::CSchema;

// ── Macro for ERRMSG-pattern FFI wrappers ──────────────────────────────────

/// Wrap a Rust Result-returning expression into the C ERRMSG convention.
/// On success: clears errmsg, returns the value.
/// On error: sets errmsg, returns $fail.
macro_rules! ffi_try {
    ($errmsg:expr, $fail:expr, $body:expr) => {{
        unsafe { clear_errmsg($errmsg) };
        match $body {
            Ok(val) => val,
            Err(e) => {
                unsafe { set_errmsg($errmsg, &e) };
                $fail
            }
        }
    }};
}

// CSchema type and conversions are in cschema.rs (always compiled).

// ── SHM functions ──────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn shinit(
    shm_basename: *const c_char,
    volume_index: usize,
    shm_size: usize,
    errmsg: *mut *mut c_char,
) -> *mut ShmHeader {
    let basename = CStr::from_ptr(shm_basename).to_string_lossy();
    ffi_try!(errmsg, ptr::null_mut(), shm::shinit(&basename, volume_index, shm_size))
}

#[no_mangle]
pub unsafe extern "C" fn shopen(
    volume_index: usize,
    errmsg: *mut *mut c_char,
) -> *mut ShmHeader {
    ffi_try!(
        errmsg,
        ptr::null_mut(),
        shm::shopen(volume_index).and_then(|opt| opt.ok_or(MorlocError::Shm("volume not found".into())))
    )
}

#[no_mangle]
pub unsafe extern "C" fn shclose(errmsg: *mut *mut c_char) -> bool {
    ffi_try!(errmsg, false, shm::shclose().map(|_| true))
}

#[no_mangle]
pub unsafe extern "C" fn shm_set_fallback_dir(dir: *const c_char) {
    if !dir.is_null() {
        let d = CStr::from_ptr(dir).to_string_lossy();
        shm::shm_set_fallback_dir(&d);
    }
}

#[no_mangle]
pub unsafe extern "C" fn shmalloc(size: usize, errmsg: *mut *mut c_char) -> *mut c_void {
    ffi_try!(errmsg, ptr::null_mut(), shm::shmalloc(size).map(|p| p as *mut c_void))
}

#[no_mangle]
pub unsafe extern "C" fn shmemcpy(
    src: *mut c_void,
    size: usize,
    errmsg: *mut *mut c_char,
) -> *mut c_void {
    ffi_try!(
        errmsg,
        ptr::null_mut(),
        shm::shmemcpy(src as *const u8, size).map(|p| p as *mut c_void)
    )
}

#[no_mangle]
pub unsafe extern "C" fn shcalloc(
    nmemb: usize,
    size: usize,
    errmsg: *mut *mut c_char,
) -> *mut c_void {
    ffi_try!(errmsg, ptr::null_mut(), shm::shcalloc(nmemb, size).map(|p| p as *mut c_void))
}

#[no_mangle]
pub unsafe extern "C" fn shrealloc(
    ptr: *mut c_void,
    size: usize,
    errmsg: *mut *mut c_char,
) -> *mut c_void {
    // TODO: implement shrealloc in shm.rs
    let _ = (ptr, size);
    set_errmsg(errmsg, &MorlocError::Shm("shrealloc not yet implemented".into()));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn shfree(ptr: *mut c_void, errmsg: *mut *mut c_char) -> bool {
    ffi_try!(errmsg, false, shm::shfree(ptr as AbsPtr).map(|_| true))
}

#[no_mangle]
pub unsafe extern "C" fn shincref(ptr: *mut c_void, errmsg: *mut *mut c_char) -> bool {
    ffi_try!(errmsg, false, shm::shincref(ptr as AbsPtr).map(|_| true))
}

// shfree_by_schema is provided by cli.c

#[no_mangle]
pub unsafe extern "C" fn total_shm_size() -> usize {
    shm::total_shm_size()
}

#[no_mangle]
pub unsafe extern "C" fn rel2abs(ptr: RelPtr, errmsg: *mut *mut c_char) -> *mut c_void {
    ffi_try!(errmsg, ptr::null_mut(), shm::rel2abs(ptr).map(|p| p as *mut c_void))
}

#[no_mangle]
pub unsafe extern "C" fn abs2rel(ptr: *mut c_void, errmsg: *mut *mut c_char) -> RelPtr {
    ffi_try!(errmsg, shm::RELNULL, shm::abs2rel(ptr as AbsPtr))
}

#[no_mangle]
pub unsafe extern "C" fn abs2shm(ptr: *mut c_void, errmsg: *mut *mut c_char) -> *mut ShmHeader {
    ffi_try!(errmsg, ptr::null_mut(), shm::abs2shm(ptr as AbsPtr))
}

#[no_mangle]
pub unsafe extern "C" fn abs2blk(ptr: *mut c_void, errmsg: *mut *mut c_char) -> *mut BlockHeader {
    clear_errmsg(errmsg);
    if ptr.is_null() {
        set_errmsg(errmsg, &MorlocError::NullPointer);
        return ptr::null_mut();
    }
    let blk = (ptr as *mut u8).sub(std::mem::size_of::<BlockHeader>()) as *mut BlockHeader;
    if (*blk).magic != shm::BLK_MAGIC {
        set_errmsg(errmsg, &MorlocError::Shm("Bad block magic".into()));
        return ptr::null_mut();
    }
    blk
}

#[no_mangle]
pub unsafe extern "C" fn vol2rel(ptr: VolPtr, shm_ptr: *const ShmHeader) -> RelPtr {
    shm::vol2rel(ptr, &*shm_ptr)
}

#[no_mangle]
pub unsafe extern "C" fn vol2abs(ptr: VolPtr, shm_ptr: *const ShmHeader) -> *mut c_void {
    shm::vol2abs(ptr, shm_ptr) as *mut c_void
}

// ── Schema functions ───────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn parse_schema(
    schema_str: *const c_char,
    errmsg: *mut *mut c_char,
) -> *mut CSchema {
    clear_errmsg(errmsg);
    if schema_str.is_null() {
        set_errmsg(errmsg, &MorlocError::NullPointer);
        return ptr::null_mut();
    }
    let s = CStr::from_ptr(schema_str).to_string_lossy();
    match schema::parse_schema(&s) {
        Ok(schema) => CSchema::from_rust(&schema),
        Err(e) => {
            set_errmsg(errmsg, &e);
            ptr::null_mut()
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn schema_to_string(schema: *const CSchema) -> *mut c_char {
    if schema.is_null() {
        return ptr::null_mut();
    }
    let rs = CSchema::to_rust(schema);
    let s = schema::schema_to_string(&rs);
    match CString::new(s) {
        Ok(cs) => cs.into_raw(),
        Err(_) => ptr::null_mut(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn free_schema(schema: *mut CSchema) {
    CSchema::free(schema);
}

#[no_mangle]
pub unsafe extern "C" fn schema_is_fixed_width(schema: *const CSchema) -> bool {
    if schema.is_null() {
        return true;
    }
    let rs = CSchema::to_rust(schema);
    rs.is_fixed_width()
}

#[no_mangle]
pub unsafe extern "C" fn schema_alignment(schema: *const CSchema) -> usize {
    if schema.is_null() {
        return 1;
    }
    let rs = CSchema::to_rust(schema);
    rs.alignment()
}

// Hash: morloc_xxh64 is provided by utility.c (via xxhash.h inline)

// ── Serialization ──────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn pack_with_schema(
    mlc: *const c_void,
    schema: *const CSchema,
    mpkptr: *mut *mut c_char,
    mpk_size: *mut usize,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    *mpkptr = ptr::null_mut();
    *mpk_size = 0;

    let rs = CSchema::to_rust(schema);
    match crate::mpack::pack_with_schema(mlc as AbsPtr, &rs) {
        Ok(data) => {
            *mpk_size = data.len();
            let buf = libc::malloc(data.len()) as *mut u8;
            if buf.is_null() {
                set_errmsg(errmsg, &MorlocError::Shm("malloc failed".into()));
                return 1;
            }
            std::ptr::copy_nonoverlapping(data.as_ptr(), buf, data.len());
            *mpkptr = buf as *mut c_char;
            0
        }
        Err(e) => {
            set_errmsg(errmsg, &e);
            1
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn pack(
    mlc: *const c_void,
    schema_str: *const c_char,
    mpkptr: *mut *mut c_char,
    mpk_size: *mut usize,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    let s = CStr::from_ptr(schema_str).to_string_lossy();
    let schema = match schema::parse_schema(&s) {
        Ok(s) => s,
        Err(e) => {
            set_errmsg(errmsg, &e);
            return 1;
        }
    };
    let cs = CSchema::from_rust(&schema);
    let result = pack_with_schema(mlc, cs, mpkptr, mpk_size, errmsg);
    free_schema(cs);
    result
}

#[no_mangle]
pub unsafe extern "C" fn unpack_with_schema(
    mpk: *const c_char,
    mpk_size: usize,
    schema: *const CSchema,
    mlcptr: *mut *mut c_void,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    *mlcptr = ptr::null_mut();

    let data = std::slice::from_raw_parts(mpk as *const u8, mpk_size);
    let rs = CSchema::to_rust(schema);
    match crate::mpack::unpack_with_schema(data, &rs) {
        Ok(ptr) => {
            *mlcptr = ptr as *mut c_void;
            0
        }
        Err(e) => {
            set_errmsg(errmsg, &e);
            1
        }
    }
}

// quoted, print_voidstar, pretty_print_voidstar, read_json_with_schema
// are provided by json.c

// ── Schema utility functions needed by C code ──────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn calculate_voidstar_size(
    data: *const c_void,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> usize {
    clear_errmsg(errmsg);
    if data.is_null() || schema.is_null() {
        return 0;
    }
    let rs = CSchema::to_rust(schema);
    match calc_voidstar_size_inner(data as *const u8, &rs) {
        Ok(size) => size,
        Err(e) => {
            set_errmsg(errmsg, &e);
            0
        }
    }
}

pub fn calc_voidstar_size_inner(
    data: *const u8,
    schema: &crate::schema::Schema,
) -> Result<usize, MorlocError> {
    use crate::schema::SerialType;
    use crate::shm::{self, Array, Tensor};

    // SAFETY: data points to voidstar data in SHM with layout described by schema.
    // We only read Array/Tensor headers and follow relptrs to compute total size.
    unsafe {
        match schema.serial_type {
            SerialType::String => {
                let arr = &*(data as *const Array);
                Ok(std::mem::size_of::<Array>() + arr.size)
            }
            SerialType::Array => {
                let arr = &*(data as *const Array);
                let mut size = std::mem::size_of::<Array>();
                if arr.size == 0 {
                    return Ok(size);
                }
                let elem_schema = &schema.parameters[0];
                let elem_width = elem_schema.width;
                size += elem_schema.alignment().saturating_sub(1);

                if schema.is_fixed_width() {
                    size += elem_width * arr.size;
                } else {
                    let elem_data = shm::rel2abs(arr.data)?;
                    for i in 0..arr.size {
                        size += calc_voidstar_size_inner(
                            elem_data.add(i * elem_width),
                            elem_schema,
                        )?;
                    }
                }
                Ok(size)
            }
            SerialType::Optional => {
                let tag = *data;
                let mut size = schema.width;
                if tag != 0 {
                    let inner_offset = schema.offsets.first().copied().unwrap_or(
                        shm::align_up(1, schema.parameters[0].alignment().max(1)),
                    );
                    let inner_total = calc_voidstar_size_inner(
                        data.add(inner_offset),
                        &schema.parameters[0],
                    )?;
                    if inner_total > schema.parameters[0].width {
                        size += inner_total - schema.parameters[0].width;
                    }
                }
                Ok(size)
            }
            SerialType::Tensor => {
                let tensor = &*(data as *const Tensor);
                let ndim = schema.offsets.first().copied().unwrap_or(0);
                let elem_width = schema.parameters[0].width;
                let mut size = std::mem::size_of::<Tensor>();
                size += schema.parameters[0].alignment().saturating_sub(1);
                size += ndim * std::mem::size_of::<i64>();
                size += schema.parameters[0].alignment().saturating_sub(1);
                size += tensor.total_elements * elem_width;
                Ok(size)
            }
            SerialType::Tuple | SerialType::Map => {
                if schema.is_fixed_width() {
                    Ok(schema.width)
                } else {
                    let mut size = schema.width;
                    for i in 0..schema.parameters.len() {
                        let elem_total = calc_voidstar_size_inner(
                            data.add(schema.offsets[i]),
                            &schema.parameters[i],
                        )?;
                        if elem_total > schema.parameters[i].width {
                            size += elem_total - schema.parameters[i].width;
                        }
                    }
                    Ok(size)
                }
            }
            _ => Ok(schema.width),
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn get_ptr(
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut c_void {
    clear_errmsg(errmsg);
    if schema.is_null() {
        return ptr::null_mut();
    }
    let rs = CSchema::to_rust(schema);
    ffi_try!(errmsg, ptr::null_mut(), shm::shmalloc(rs.width).map(|p| p as *mut c_void))
}
