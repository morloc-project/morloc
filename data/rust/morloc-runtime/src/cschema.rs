//! C-compatible Schema type for FFI.
//! This module is always compiled (even with no-ffi-exports feature).

use std::ffi::{c_char, CStr, CString};
use std::ptr;

use crate::schema::{Schema, SerialType};

/// C-compatible Schema struct matching the C `Schema` layout.
#[repr(C)]
pub struct CSchema {
    pub serial_type: u32,
    pub size: usize,
    pub width: usize,
    pub offsets: *mut usize,
    pub hint: *mut c_char,
    pub parameters: *mut *mut CSchema,
    pub keys: *mut *mut c_char,
}

impl CSchema {
    pub fn from_rust(schema: &Schema) -> *mut CSchema {
        let cs = Box::new(CSchema {
            serial_type: schema.serial_type as u32,
            size: schema.size,
            width: schema.width,
            offsets: if schema.offsets.is_empty() {
                ptr::null_mut()
            } else {
                let mut v = schema.offsets.clone().into_boxed_slice();
                let p = v.as_mut_ptr();
                std::mem::forget(v);
                p
            },
            hint: match &schema.hint {
                Some(s) => CString::new(s.as_str()).unwrap_or_default().into_raw(),
                None => ptr::null_mut(),
            },
            parameters: if schema.parameters.is_empty() {
                ptr::null_mut()
            } else {
                let mut ptrs: Vec<*mut CSchema> = schema
                    .parameters
                    .iter()
                    .map(|p| CSchema::from_rust(p))
                    .collect();
                let p = ptrs.as_mut_ptr();
                std::mem::forget(ptrs);
                p
            },
            keys: if schema.keys.is_empty() {
                ptr::null_mut()
            } else {
                let mut ptrs: Vec<*mut c_char> = schema
                    .keys
                    .iter()
                    .map(|k| CString::new(k.as_str()).unwrap_or_default().into_raw())
                    .collect();
                let p = ptrs.as_mut_ptr();
                std::mem::forget(ptrs);
                p
            },
        });
        Box::into_raw(cs)
    }

    /// Convert a C-allocated CSchema to a Rust Schema by deep-copying all data.
    ///
    /// # Safety
    /// `cs` must be null or a valid pointer to a CSchema allocated by `from_rust`
    /// or equivalent C code. All child pointers must be valid for `cs.size` entries.
    pub unsafe fn to_rust(cs: *const CSchema) -> Schema {
        if cs.is_null() {
            return Schema::primitive(SerialType::Nil);
        }
        let cs = &*cs;
        // SAFETY: SerialType is #[repr(u32)] and cs.serial_type was set from a valid SerialType.
        let serial_type = std::mem::transmute::<u32, SerialType>(cs.serial_type);

        let offsets = if cs.offsets.is_null() || cs.size == 0 {
            Vec::new()
        } else {
            let n = match serial_type {
                SerialType::Tuple | SerialType::Map => cs.size,
                SerialType::Optional | SerialType::Tensor => 1,
                _ => 0,
            };
            if n > 0 {
                std::slice::from_raw_parts(cs.offsets, n).to_vec()
            } else {
                Vec::new()
            }
        };

        let parameters = if cs.parameters.is_null() || cs.size == 0 {
            Vec::new()
        } else {
            (0..cs.size)
                .map(|i| CSchema::to_rust(*cs.parameters.add(i)))
                .collect()
        };

        let keys = if cs.keys.is_null() || cs.size == 0 {
            Vec::new()
        } else {
            (0..cs.size)
                .filter_map(|i| {
                    let p = *cs.keys.add(i);
                    if p.is_null() { None }
                    else { Some(CStr::from_ptr(p).to_string_lossy().into_owned()) }
                })
                .collect()
        };

        let hint = if cs.hint.is_null() {
            None
        } else {
            Some(CStr::from_ptr(cs.hint).to_string_lossy().into_owned())
        };

        Schema {
            serial_type,
            size: cs.size,
            width: cs.width,
            offsets,
            hint,
            parameters,
            keys,
        }
    }

    /// Free a CSchema and all its children (same logic as ffi::free_schema).
    ///
    /// # Safety
    /// `schema` must be null or a valid pointer previously returned by `from_rust`.
    pub unsafe fn free(schema: *mut CSchema) {
        if schema.is_null() { return; }
        let cs = Box::from_raw(schema);
        // SAFETY: cs.serial_type was set from a valid SerialType in from_rust.
        let st = std::mem::transmute::<u32, SerialType>(cs.serial_type);
        if !cs.offsets.is_null() {
            let n = match st {
                SerialType::Tuple | SerialType::Map => cs.size,
                SerialType::Optional | SerialType::Tensor => 1,
                _ => 0,
            };
            if n > 0 { let _ = Vec::from_raw_parts(cs.offsets, n, n); }
        }
        if !cs.hint.is_null() { let _ = CString::from_raw(cs.hint); }
        if !cs.parameters.is_null() && cs.size > 0 {
            let ptrs = Vec::from_raw_parts(cs.parameters, cs.size, cs.size);
            for p in ptrs { CSchema::free(p); }
        }
        if !cs.keys.is_null() && cs.size > 0 {
            let ptrs = Vec::from_raw_parts(cs.keys, cs.size, cs.size);
            for p in ptrs { if !p.is_null() { let _ = CString::from_raw(p); } }
        }
    }
}
