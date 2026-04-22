//! C ABI wrappers for manifest parsing and discovery JSON.
//!
//! This file mirrors the manifest data model into raw `#[repr(C)]`
//! structs that the daemon and slurm subsystems read via FFI from C
//! code. It is **not** the canonical Rust deserializer of the manifest
//! schema -- that lives in
//! `data/rust/morloc-nexus/src/manifest.rs`, which has full doc
//! comments describing the v2 manifest schema, every field's purpose,
//! and which slots are reserved for future expansion.
//!
//! The split exists for two reasons:
//!
//! 1. **C ABI stability.** The C structs here have the original v1
//!    field layout (flat `arg_schemas`, `return_schema`, `return_type`,
//!    `return_desc`, `build_dir`, `version`) so that downstream C
//!    callers (the daemon, the slurm bridge, any third-party FFI
//!    consumers) don't break when the JSON schema evolves. The
//!    `parse_manifest` function below reads the new v2 JSON shape and
//!    populates these legacy C struct fields, acting as a translation
//!    layer.
//!
//! 2. **Build-time decoupling.** The morloc-runtime crate needs to
//!    consume manifests without depending on the morloc-nexus crate.
//!    Sharing a Rust-level data model would create a circular
//!    dependency between the two crates.
//!
//! When the v2 schema gains new fields (constraints, metadata, etc.),
//! the canonical Rust model in `morloc-nexus/src/manifest.rs` is
//! updated first. This file gets new C struct fields only when a C-side
//! consumer needs them; otherwise the new JSON keys are silently
//! ignored here, which is the correct forward-compatible behavior.

use std::ffi::{c_char, c_void, CStr, CString};
use std::ptr;

use crate::cschema::CSchema;
use crate::error::{clear_errmsg, set_errmsg, MorlocError};

// -- C-compatible types matching eval.h ---------------------------------------

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MorlocExpressionType {
    Dat = 0,
    App = 1,
    Lam = 2,
    Bnd = 3,
    Pat = 4,
    Fmt = 5,
    Show = 6,
    Read = 7,
    Hash = 8,
    Save = 9,
    Load = 10,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MorlocAppExpressionType {
    Pattern = 0,
    Lambda = 1,
    Format = 2,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MorlocPatternType {
    ByKey = 0,
    ByIndex = 1,
    End = 2,
}

#[repr(C)]
pub union PatternFields {
    pub indices: *mut usize,
    pub keys: *mut *mut c_char,
}

#[repr(C)]
pub struct MorlocPattern {
    pub ptype: MorlocPatternType,
    pub size: usize,
    pub fields: PatternFields,
    pub selectors: *mut *mut MorlocPattern,
}

#[repr(C)]
pub union Primitive {
    pub s: *mut c_char,
    pub z: u8,
    pub b: bool,
    pub i1: i8,
    pub i2: i16,
    pub i4: i32,
    pub i8_: i64,
    pub u1: u8,
    pub u2: u16,
    pub u4: u32,
    pub u8_: u64,
    pub f4: f32,
    pub f8: f64,
}

#[repr(C)]
pub struct MorlocDataArray {
    pub schema: *mut CSchema,
    pub size: usize,
    pub values: *mut *mut MorlocExpression,
}

#[repr(C)]
// Primitive contains a pointer (s: *mut c_char), so DataUnion uses ManuallyDrop
#[repr(C)]
pub union DataUnion {
    pub lit_val: std::mem::ManuallyDrop<Primitive>,
    pub tuple_val: *mut *mut MorlocExpression,
    pub array_val: *mut MorlocDataArray,
    pub voidstar: *mut c_void,
}

#[repr(C)]
pub struct MorlocData {
    pub is_voidstar: bool,
    pub data: DataUnion,
}

#[repr(C)]
pub union AppFunction {
    pub pattern: *mut MorlocPattern,
    pub lambda: *mut MorlocLamExpression,
    pub fmt: *mut *mut c_char,
}

#[repr(C)]
pub struct MorlocAppExpression {
    pub atype: MorlocAppExpressionType,
    pub function: AppFunction,
    pub args: *mut *mut MorlocExpression,
    pub nargs: usize,
}

#[repr(C)]
pub struct MorlocLamExpression {
    pub nargs: usize,
    pub args: *mut *mut c_char,
    pub body: *mut MorlocExpression,
}

#[repr(C)]
pub struct MorlocSaveExpression {
    pub format: *mut c_char,
    pub value: *mut MorlocExpression,
    pub path: *mut MorlocExpression,
}

#[repr(C)]
pub union ExprUnion {
    pub app_expr: *mut MorlocAppExpression,
    pub lam_expr: *mut MorlocLamExpression,
    pub bnd_expr: *mut c_char,
    pub interpolation: *mut *mut c_char,
    pub pattern_expr: *mut MorlocPattern,
    pub data_expr: *mut MorlocData,
    pub unary_expr: *mut MorlocExpression,
    pub save_expr: *mut MorlocSaveExpression,
}

#[repr(C)]
pub struct MorlocExpression {
    pub etype: MorlocExpressionType,
    pub schema: *mut CSchema,
    pub expr: ExprUnion,
}

// -- C-ABI mirror of morloc-manifest v2 ---------------------------------------
//
// These #[repr(C)] structs are the in-memory layout that daemon_ffi /
// router_ffi / slurm_ffi consume via raw pointers. They mirror the
// shape of morloc_manifest's Rust types one-for-one (modulo C-string
// encoding) -- when the Rust schema gains a new field, it's added here
// too as a parallel C field. There is no longer any "translation" or
// reshape layer; parse_manifest below is a near-1:1 walker.
//
// Conventions:
// - C-string fields are owned by the manifest and freed by free_manifest.
// - Array fields use a (pointer, count) pair (e.g. desc + n_desc).
// - The "constraints" and "metadata" extension slots described in
//   morloc-manifest's docs are mirrored here so daemon-side constraint
//   enforcement can later read them without another C ABI break.
// - metadata is serialized as JSON text (`metadata_json`) because the
//   C side has no natural map type and the slot is reserved for now.

#[repr(C)]
pub struct ManifestBuild {
    pub path: *mut c_char,
    pub time: i64,
    pub morloc_version: *mut c_char,
}

#[repr(C)]
pub struct ManifestConstraint {
    /// Constraint discriminator (e.g. "kind", "min", "regex").
    pub ctype: *mut c_char,
    /// JSON-encoded payload for the constraint, or NULL when the
    /// constraint type carries no value (e.g. "non_empty").
    pub value_json: *mut c_char,
}

#[repr(C)]
pub struct ManifestPool {
    pub lang: *mut c_char,
    pub exec: *mut *mut c_char, // NULL-terminated
    pub socket: *mut c_char,
    /// JSON-encoded pool-level metadata. Reserved.
    pub metadata_json: *mut c_char,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ManifestArgKind {
    Pos = 0,
    Opt = 1,
    Flag = 2,
    Grp = 3,
}

#[repr(C)]
pub struct ManifestGrpEntry {
    pub key: *mut c_char,
    pub arg: *mut ManifestArg,
}

#[repr(C)]
pub struct ManifestArg {
    pub kind: ManifestArgKind,
    /// Per-arg morloc serialization schema. NULL for flags. Group
    /// entries also have NULL here (the group's top-level schema
    /// covers them).
    pub schema: *mut c_char,
    /// User-facing type name. NULL for flags.
    pub type_desc: *mut c_char,
    pub metavar: *mut c_char,
    pub quoted: bool,
    pub short_opt: c_char,
    pub long_opt: *mut c_char,
    pub long_rev: *mut c_char,
    pub default_val: *mut c_char,
    /// NULL-terminated array of description lines.
    pub desc: *mut *mut c_char,
    pub n_desc: usize,
    /// Array of ManifestConstraint owned by this arg.
    pub constraints: *mut ManifestConstraint,
    pub n_constraints: usize,
    /// Group sub-fields (only meaningful when kind == Grp).
    pub grp_short: c_char,
    pub grp_long: *mut c_char,
    pub entries: *mut ManifestGrpEntry,
    pub n_entries: usize,
    /// JSON-encoded per-arg metadata. Reserved.
    pub metadata_json: *mut c_char,
}

#[repr(C)]
pub struct ManifestReturn {
    pub schema: *mut c_char,
    pub type_desc: *mut c_char,
    pub desc: *mut *mut c_char,
    pub n_desc: usize,
    pub constraints: *mut ManifestConstraint,
    pub n_constraints: usize,
    pub metadata_json: *mut c_char,
}

#[repr(C)]
pub struct ManifestCmdGroup {
    pub name: *mut c_char,
    pub desc: *mut *mut c_char,
    pub n_desc: usize,
    pub metadata_json: *mut c_char,
}

#[repr(C)]
pub struct ManifestCommand {
    pub name: *mut c_char,
    pub is_pure: bool,
    pub mid: u32,
    pub pool_index: usize,
    pub needed_pools: *mut usize,
    pub n_needed_pools: usize,
    pub desc: *mut *mut c_char,
    pub n_desc: usize,
    pub args: *mut ManifestArg,
    pub n_args: usize,
    /// Return-value descriptor as a sub-struct (replaces v1's flat
    /// return_schema/return_type/return_desc fields).
    pub ret: ManifestReturn,
    pub constraints: *mut ManifestConstraint,
    pub n_constraints: usize,
    pub expr: *mut MorlocExpression,
    pub group: *mut c_char,
    pub metadata_json: *mut c_char,
}

#[repr(C)]
pub struct ManifestService {
    pub stype: *mut c_char,
    pub host: *mut c_char,
    pub port: i32,
    pub socket: *mut c_char,
    pub metadata_json: *mut c_char,
}

#[repr(C)]
pub struct Manifest {
    pub name: *mut c_char,
    pub build: ManifestBuild,
    pub pools: *mut ManifestPool,
    pub n_pools: usize,
    pub commands: *mut ManifestCommand,
    pub n_commands: usize,
    pub groups: *mut ManifestCmdGroup,
    pub n_groups: usize,
    pub service: *mut ManifestService,
    pub metadata_json: *mut c_char,
}

impl ManifestCommand {
    /// Build a transient NULL-terminated array of schema strings for
    /// the command's args, in declaration order. The caller owns the
    /// outer array allocation but NOT the inner C strings (they
    /// remain owned by the ManifestArg objects). Use
    /// `libc::free(arr as *mut c_void)` to release the outer array
    /// when done.
    ///
    /// The array has one entry per arg INCLUDING flags. Flags have a
    /// per-arg `schema` field of NULL on the v2 ManifestArg, but the
    /// legacy callers (e.g. make_call_packet_from_cli) expect a slot
    /// per arg position to keep index alignment with the parallel
    /// args array; we substitute "b" (the boolean schema) for flags
    /// so dispatch reads the flag value as a Bool, matching v1
    /// behavior.
    pub unsafe fn build_arg_schemas_array(&self) -> *mut *mut c_char {
        let n = self.n_args;
        let arr = libc::calloc(n + 1, std::mem::size_of::<*mut c_char>()) as *mut *mut c_char;
        for i in 0..n {
            let arg = &*self.args.add(i);
            *arr.add(i) = if arg.kind == ManifestArgKind::Flag || arg.schema.is_null() {
                // Flag schema fallback: use the static "b" string. This
                // pointer is NOT freed by the caller (it's a global
                // string literal), but since the caller only frees the
                // OUTER array, this is fine. We use a per-call CString
                // leak so each call has a distinct C-string buffer the
                // caller doesn't try to free with the per-arg owners.
                // Cleaner: we just point to the existing per-arg
                // schema slot if non-null, else fall through. Most
                // flag args won't be hit by legacy callers anyway.
                static FLAG_SCHEMA: &[u8] = b"b\0";
                FLAG_SCHEMA.as_ptr() as *mut c_char
            } else {
                arg.schema
            };
        }
        *arr.add(n) = ptr::null_mut();
        arr
    }
}

// -- Helpers ------------------------------------------------------------------

unsafe fn c_strdup(s: &str) -> *mut c_char {
    match CString::new(s) {
        Ok(cs) => libc::strdup(cs.as_ptr()),
        Err(_) => ptr::null_mut(),
    }
}

unsafe fn nullable_strdup(s: Option<&str>) -> *mut c_char {
    match s {
        Some(s) => c_strdup(s),
        None => ptr::null_mut(),
    }
}

// -- Expression builder (using serde_json::Value) -----------------------------

unsafe fn build_pattern(jp: &serde_json::Value) -> Result<*mut MorlocPattern, MorlocError> {
    let ptype = jp.get("type").and_then(|v| v.as_str()).ok_or_else(|| MorlocError::Other("Pattern missing 'type' field".into()))?;

    if ptype == "end" {
        // make_morloc_pattern_end - call C function
        extern "C" {
            fn make_morloc_pattern_end() -> *mut MorlocPattern;
        }
        return Ok(make_morloc_pattern_end());
    }

    let sels = jp.get("selectors").and_then(|v| v.as_array());
    let n = sels.map(|a| a.len()).unwrap_or(0);

    let pat = libc::calloc(1, std::mem::size_of::<MorlocPattern>()) as *mut MorlocPattern;
    (*pat).size = n;
    (*pat).selectors = libc::calloc(n, std::mem::size_of::<*mut MorlocPattern>()) as *mut *mut MorlocPattern;

    if ptype == "idx" {
        (*pat).ptype = MorlocPatternType::ByIndex;
        (*pat).fields.indices = libc::calloc(n, std::mem::size_of::<usize>()) as *mut usize;
        if let Some(sels) = sels {
            for (i, sel) in sels.iter().enumerate() {
                *(*pat).fields.indices.add(i) = sel.get("index").and_then(|v| v.as_f64()).unwrap_or(0.0) as usize;
                *(*pat).selectors.add(i) = build_pattern(sel.get("sub").unwrap_or(&serde_json::Value::Null))?;
            }
        }
    } else if ptype == "key" {
        (*pat).ptype = MorlocPatternType::ByKey;
        (*pat).fields.keys = libc::calloc(n, std::mem::size_of::<*mut c_char>()) as *mut *mut c_char;
        if let Some(sels) = sels {
            for (i, sel) in sels.iter().enumerate() {
                let key = sel.get("key").and_then(|v| v.as_str()).unwrap_or("");
                *(*pat).fields.keys.add(i) = c_strdup(key);
                *(*pat).selectors.add(i) = build_pattern(sel.get("sub").unwrap_or(&serde_json::Value::Null))?;
            }
        }
    } else {
        return Err(MorlocError::Other(format!("Unknown pattern type: {}", ptype)));
    }

    Ok(pat)
}

unsafe fn build_expr(je: &serde_json::Value) -> Result<*mut MorlocExpression, MorlocError> {
    let tag = je.get("tag").and_then(|v| v.as_str()).ok_or_else(|| MorlocError::Other("Expression missing 'tag' field".into()))?;

    extern "C" {
        fn parse_schema(s: *const c_char, errmsg: *mut *mut c_char) -> *mut CSchema;
        fn make_morloc_literal(schema: *const c_char, prim: Primitive, errmsg: *mut *mut c_char) -> *mut MorlocExpression;
        fn make_morloc_bound_var(schema: *const c_char, var: *mut c_char, errmsg: *mut *mut c_char) -> *mut MorlocExpression;
        fn make_morloc_pattern(schema: *const c_char, pat: *mut MorlocPattern, errmsg: *mut *mut c_char) -> *mut MorlocExpression;
    }

    let mut err: *mut c_char = ptr::null_mut();

    match tag {
        "lit" => {
            let schema = je.get("schema").and_then(|v| v.as_str()).unwrap_or("");
            let lt = je.get("lit_type").and_then(|v| v.as_str()).unwrap_or("");
            let val = je.get("value").and_then(|v| v.as_str()).unwrap_or("0");
            let mut prim: Primitive = std::mem::zeroed();

            match lt {
                "f4" => prim.f4 = val.parse::<f32>().unwrap_or(0.0),
                "f8" => prim.f8 = val.parse::<f64>().unwrap_or(0.0),
                "i1" => prim.i1 = val.parse::<i8>().unwrap_or(0),
                "i2" => prim.i2 = val.parse::<i16>().unwrap_or(0),
                "i4" => prim.i4 = val.parse::<i32>().unwrap_or(0),
                "i8" => prim.i8_ = val.parse::<i64>().unwrap_or(0),
                "u1" => prim.u1 = val.parse::<u8>().unwrap_or(0),
                "u2" => prim.u2 = val.parse::<u16>().unwrap_or(0),
                "u4" => prim.u4 = val.parse::<u32>().unwrap_or(0),
                "u8" => prim.u8_ = val.parse::<u64>().unwrap_or(0),
                "b" => prim.b = val != "0",
                "z" => prim.z = 0,
                _ => return Err(MorlocError::Other(format!("Unknown lit_type: {}", lt))),
            }

            let c_schema = CString::new(schema).unwrap_or_default();
            let result = make_morloc_literal(c_schema.as_ptr(), prim, &mut err);
            if !err.is_null() {
                let msg = CStr::from_ptr(err).to_string_lossy().into_owned();
                libc::free(err as *mut c_void);
                return Err(MorlocError::Other(msg));
            }
            Ok(result)
        }

        "str" => {
            let schema = je.get("schema").and_then(|v| v.as_str()).unwrap_or("");
            let val = je.get("value").and_then(|v| v.as_str()).unwrap_or("");
            let mut prim: Primitive = std::mem::zeroed();
            prim.s = c_strdup(val);
            let c_schema = CString::new(schema).unwrap_or_default();
            let result = make_morloc_literal(c_schema.as_ptr(), prim, &mut err);
            if !err.is_null() {
                let msg = CStr::from_ptr(err).to_string_lossy().into_owned();
                libc::free(err as *mut c_void);
                return Err(MorlocError::Other(msg));
            }
            Ok(result)
        }

        "container" => {
            let schema_str = je.get("schema").and_then(|v| v.as_str()).unwrap_or("");
            let elems = je.get("elements").and_then(|v| v.as_array());
            let n = elems.map(|a| a.len()).unwrap_or(0);

            let c_schema_str = CString::new(schema_str).unwrap_or_default();
            let schema = parse_schema(c_schema_str.as_ptr(), &mut err);
            if !err.is_null() {
                let msg = CStr::from_ptr(err).to_string_lossy().into_owned();
                libc::free(err as *mut c_void);
                return Err(MorlocError::Other(msg));
            }

            let values = libc::calloc(n, std::mem::size_of::<*mut MorlocExpression>()) as *mut *mut MorlocExpression;
            if let Some(elems) = elems {
                for (i, elem) in elems.iter().enumerate() {
                    *values.add(i) = build_expr(elem)?;
                }
            }

            let data = libc::calloc(1, std::mem::size_of::<MorlocData>()) as *mut MorlocData;
            (*data).is_voidstar = false;

            let schema_type = (*schema).serial_type;
            // MORLOC_ARRAY = 14, MORLOC_TUPLE = 15, MORLOC_MAP = 16
            if schema_type == 14 { // Array
                let arr = libc::calloc(1, std::mem::size_of::<MorlocDataArray>()) as *mut MorlocDataArray;
                (*arr).schema = if (*schema).size > 0 && !(*schema).parameters.is_null() { *(*schema).parameters } else { ptr::null_mut() };
                (*arr).size = n;
                (*arr).values = values;
                (*data).data.array_val = arr;
            } else if schema_type == 15 || schema_type == 16 { // Tuple or Map
                (*data).data.tuple_val = values;
            } else {
                libc::free(values as *mut c_void);
                libc::free(data as *mut c_void);
                CSchema::free(schema);
                return Err(MorlocError::Other("Container schema is not a container type".into()));
            }

            let expr = libc::calloc(1, std::mem::size_of::<MorlocExpression>()) as *mut MorlocExpression;
            (*expr).etype = MorlocExpressionType::Dat;
            (*expr).schema = schema;
            (*expr).expr.data_expr = data;
            Ok(expr)
        }

        "app" => {
            let schema_str = je.get("schema").and_then(|v| v.as_str()).unwrap_or("");
            let jargs = je.get("args").and_then(|v| v.as_array());
            let n = jargs.map(|a| a.len()).unwrap_or(0);

            let c_schema_str = CString::new(schema_str).unwrap_or_default();
            let schema = parse_schema(c_schema_str.as_ptr(), &mut err);
            if !err.is_null() {
                let msg = CStr::from_ptr(err).to_string_lossy().into_owned();
                libc::free(err as *mut c_void);
                return Err(MorlocError::Other(msg));
            }

            let func = build_expr(je.get("func").unwrap_or(&serde_json::Value::Null))?;
            let args = libc::calloc(n, std::mem::size_of::<*mut MorlocExpression>()) as *mut *mut MorlocExpression;
            if let Some(jargs) = jargs {
                for (i, a) in jargs.iter().enumerate() {
                    *args.add(i) = build_expr(a)?;
                }
            }

            let app = libc::calloc(1, std::mem::size_of::<MorlocAppExpression>()) as *mut MorlocAppExpression;
            match (*func).etype {
                MorlocExpressionType::Pat => {
                    (*app).atype = MorlocAppExpressionType::Pattern;
                    (*app).function.pattern = (*func).expr.pattern_expr;
                }
                MorlocExpressionType::Lam => {
                    (*app).atype = MorlocAppExpressionType::Lambda;
                    (*app).function.lambda = (*func).expr.lam_expr;
                }
                MorlocExpressionType::Fmt => {
                    (*app).atype = MorlocAppExpressionType::Format;
                    (*app).function.fmt = (*func).expr.interpolation;
                }
                _ => {
                    return Err(MorlocError::Other(format!("Invalid function in app expression (type={:?})", (*func).etype)));
                }
            }
            (*app).args = args;
            (*app).nargs = n;

            let expr = libc::calloc(1, std::mem::size_of::<MorlocExpression>()) as *mut MorlocExpression;
            (*expr).etype = MorlocExpressionType::App;
            (*expr).schema = schema;
            (*expr).expr.app_expr = app;
            Ok(expr)
        }

        "lambda" => {
            let jvars = je.get("vars").and_then(|v| v.as_array());
            let n = jvars.map(|a| a.len()).unwrap_or(0);

            let body = build_expr(je.get("body").unwrap_or(&serde_json::Value::Null))?;
            let vars = libc::calloc(n, std::mem::size_of::<*mut c_char>()) as *mut *mut c_char;
            if let Some(jvars) = jvars {
                for (i, v) in jvars.iter().enumerate() {
                    *vars.add(i) = c_strdup(v.as_str().unwrap_or(""));
                }
            }

            let lam = libc::calloc(1, std::mem::size_of::<MorlocLamExpression>()) as *mut MorlocLamExpression;
            (*lam).nargs = n;
            (*lam).args = vars;
            (*lam).body = body;

            let expr = libc::calloc(1, std::mem::size_of::<MorlocExpression>()) as *mut MorlocExpression;
            (*expr).etype = MorlocExpressionType::Lam;
            (*expr).schema = ptr::null_mut();
            (*expr).expr.lam_expr = lam;
            Ok(expr)
        }

        "bound" => {
            let schema = je.get("schema").and_then(|v| v.as_str()).unwrap_or("");
            let var = je.get("var").and_then(|v| v.as_str()).unwrap_or("");
            let c_schema = CString::new(schema).unwrap_or_default();
            let c_var = c_strdup(var);
            let result = make_morloc_bound_var(c_schema.as_ptr(), c_var, &mut err);
            if !err.is_null() {
                let msg = CStr::from_ptr(err).to_string_lossy().into_owned();
                libc::free(err as *mut c_void);
                return Err(MorlocError::Other(msg));
            }
            Ok(result)
        }

        "show" | "read" | "hash" | "load" => {
            let schema_str = je.get("schema").and_then(|v| v.as_str()).unwrap_or("");
            let c_schema_str = CString::new(schema_str).unwrap_or_default();
            let schema = parse_schema(c_schema_str.as_ptr(), &mut err);
            if !err.is_null() {
                let msg = CStr::from_ptr(err).to_string_lossy().into_owned();
                libc::free(err as *mut c_void);
                return Err(MorlocError::Other(msg));
            }
            let child = build_expr(je.get("child").unwrap_or(&serde_json::Value::Null))?;
            let expr = libc::calloc(1, std::mem::size_of::<MorlocExpression>()) as *mut MorlocExpression;
            (*expr).etype = match tag {
                "show" => MorlocExpressionType::Show,
                "read" => MorlocExpressionType::Read,
                "hash" => MorlocExpressionType::Hash,
                "load" => MorlocExpressionType::Load,
                _ => unreachable!(),
            };
            (*expr).schema = schema;
            (*expr).expr.unary_expr = child;
            Ok(expr)
        }

        "save" => {
            let schema_str = je.get("schema").and_then(|v| v.as_str()).unwrap_or("");
            let c_schema_str = CString::new(schema_str).unwrap_or_default();
            let schema = parse_schema(c_schema_str.as_ptr(), &mut err);
            if !err.is_null() {
                let msg = CStr::from_ptr(err).to_string_lossy().into_owned();
                libc::free(err as *mut c_void);
                return Err(MorlocError::Other(msg));
            }
            let fmt_str = je.get("format").and_then(|v| v.as_str()).unwrap_or("voidstar");
            let c_fmt = CString::new(fmt_str).unwrap_or_default();
            let value = build_expr(je.get("value").unwrap_or(&serde_json::Value::Null))?;
            let path = build_expr(je.get("path").unwrap_or(&serde_json::Value::Null))?;
            let save = libc::calloc(1, std::mem::size_of::<MorlocSaveExpression>()) as *mut MorlocSaveExpression;
            (*save).format = c_fmt.into_raw();
            (*save).value = value;
            (*save).path = path;
            let expr = libc::calloc(1, std::mem::size_of::<MorlocExpression>()) as *mut MorlocExpression;
            (*expr).etype = MorlocExpressionType::Save;
            (*expr).schema = schema;
            (*expr).expr.save_expr = save;
            Ok(expr)
        }

        "interpolation" => {
            let schema_str = je.get("schema").and_then(|v| v.as_str()).unwrap_or("");
            let jstrs = je.get("strings").and_then(|v| v.as_array());
            let n = jstrs.map(|a| a.len()).unwrap_or(0);

            let c_schema_str = CString::new(schema_str).unwrap_or_default();
            let schema = parse_schema(c_schema_str.as_ptr(), &mut err);
            if !err.is_null() {
                let msg = CStr::from_ptr(err).to_string_lossy().into_owned();
                libc::free(err as *mut c_void);
                return Err(MorlocError::Other(msg));
            }

            let strings = libc::calloc(n + 1, std::mem::size_of::<*mut c_char>()) as *mut *mut c_char;
            if let Some(jstrs) = jstrs {
                for (i, s) in jstrs.iter().enumerate() {
                    *strings.add(i) = c_strdup(s.as_str().unwrap_or(""));
                }
            }

            let expr = libc::calloc(1, std::mem::size_of::<MorlocExpression>()) as *mut MorlocExpression;
            (*expr).etype = MorlocExpressionType::Fmt;
            (*expr).schema = schema;
            (*expr).expr.interpolation = strings;
            Ok(expr)
        }

        "pattern" => {
            let schema_str = je.get("schema").and_then(|v| v.as_str()).unwrap_or("");
            let pat = build_pattern(je.get("pattern").unwrap_or(&serde_json::Value::Null))?;
            let c_schema = CString::new(schema_str).unwrap_or_default();
            let result = make_morloc_pattern(c_schema.as_ptr(), pat, &mut err);
            if !err.is_null() {
                let msg = CStr::from_ptr(err).to_string_lossy().into_owned();
                libc::free(err as *mut c_void);
                return Err(MorlocError::Other(msg));
            }
            Ok(result)
        }

        _ => Err(MorlocError::Other(format!("Unknown expression tag: {}", tag))),
    }
}

// -- build_manifest_expr ------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn build_manifest_expr(
    json_str: *const c_char,
    errmsg: *mut *mut c_char,
) -> *mut MorlocExpression {
    clear_errmsg(errmsg);
    let s = CStr::from_ptr(json_str).to_string_lossy();
    let jv: serde_json::Value = match serde_json::from_str(&s) {
        Ok(v) => v,
        Err(e) => {
            set_errmsg(errmsg, &MorlocError::Other(format!("Failed to parse expression JSON: {}", e)));
            return ptr::null_mut();
        }
    };
    match build_expr(&jv) {
        Ok(expr) => expr,
        Err(e) => {
            set_errmsg(errmsg, &e);
            ptr::null_mut()
        }
    }
}

// -- C-ABI population from morloc-manifest Rust types -------------------------
//
// parse_manifest is now a thin shell over morloc_manifest::parse_manifest
// (which does all the JSON parsing, version checking, and serde
// validation). The walker functions below convert the Rust-native
// structs into owned C structs, mirroring the v2 schema field-for-field.

unsafe fn populate_constraint(dst: *mut ManifestConstraint, src: &morloc_manifest::Constraint) {
    (*dst).ctype = c_strdup(&src.ctype);
    (*dst).value_json = match &src.value {
        Some(v) => c_strdup(&v.to_string()),
        None => ptr::null_mut(),
    };
}

unsafe fn populate_constraints(
    src: &[morloc_manifest::Constraint],
) -> (*mut ManifestConstraint, usize) {
    if src.is_empty() {
        return (ptr::null_mut(), 0);
    }
    let arr =
        libc::calloc(src.len(), std::mem::size_of::<ManifestConstraint>()) as *mut ManifestConstraint;
    for (i, c) in src.iter().enumerate() {
        populate_constraint(arr.add(i), c);
    }
    (arr, src.len())
}

unsafe fn populate_metadata(src: &morloc_manifest::Metadata) -> *mut c_char {
    if src.is_empty() {
        c_strdup("{}")
    } else {
        let s = serde_json::to_string(src).unwrap_or_else(|_| "{}".into());
        c_strdup(&s)
    }
}

/// Convert a Vec<String> to a NULL-terminated array of C strings,
/// and return (pointer, count). Caller owns the allocation.
unsafe fn populate_str_vec(src: &[String]) -> (*mut *mut c_char, usize) {
    let n = src.len();
    let arr = libc::calloc(n + 1, std::mem::size_of::<*mut c_char>()) as *mut *mut c_char;
    for (i, s) in src.iter().enumerate() {
        *arr.add(i) = c_strdup(s);
    }
    *arr.add(n) = ptr::null_mut();
    (arr, n)
}

unsafe fn populate_arg(dst: *mut ManifestArg, src: &morloc_manifest::Arg) {
    use morloc_manifest::Arg;
    match src {
        Arg::Positional {
            schema,
            type_desc,
            metavar,
            quoted,
            desc,
            constraints,
            ..
        } => {
            (*dst).kind = ManifestArgKind::Pos;
            (*dst).schema = nullable_strdup(schema.as_deref());
            (*dst).type_desc = nullable_strdup(type_desc.as_deref());
            (*dst).metavar = nullable_strdup(metavar.as_deref());
            (*dst).quoted = *quoted;
            let (d, n) = populate_str_vec(desc);
            (*dst).desc = d;
            (*dst).n_desc = n;
            let (cs, nc) = populate_constraints(constraints);
            (*dst).constraints = cs;
            (*dst).n_constraints = nc;
            (*dst).metadata_json = c_strdup("{}");
        }
        Arg::Optional {
            schema,
            type_desc,
            metavar,
            quoted,
            short_opt,
            long_opt,
            default_val,
            desc,
            constraints,
            ..
        } => {
            (*dst).kind = ManifestArgKind::Opt;
            (*dst).schema = nullable_strdup(schema.as_deref());
            (*dst).type_desc = nullable_strdup(type_desc.as_deref());
            (*dst).metavar = nullable_strdup(metavar.as_deref());
            (*dst).quoted = *quoted;
            (*dst).short_opt = short_opt
                .as_ref()
                .and_then(|s| s.as_bytes().first().copied())
                .map(|b| b as c_char)
                .unwrap_or(0);
            (*dst).long_opt = nullable_strdup(long_opt.as_deref());
            (*dst).default_val = nullable_strdup(default_val.as_deref());
            let (d, n) = populate_str_vec(desc);
            (*dst).desc = d;
            (*dst).n_desc = n;
            let (cs, nc) = populate_constraints(constraints);
            (*dst).constraints = cs;
            (*dst).n_constraints = nc;
            (*dst).metadata_json = c_strdup("{}");
        }
        Arg::Flag {
            short_opt,
            long_opt,
            long_rev,
            default_val,
            desc,
            ..
        } => {
            (*dst).kind = ManifestArgKind::Flag;
            (*dst).short_opt = short_opt
                .as_ref()
                .and_then(|s| s.as_bytes().first().copied())
                .map(|b| b as c_char)
                .unwrap_or(0);
            (*dst).long_opt = nullable_strdup(long_opt.as_deref());
            (*dst).long_rev = nullable_strdup(long_rev.as_deref());
            (*dst).default_val = nullable_strdup(default_val.as_deref());
            let (d, n) = populate_str_vec(desc);
            (*dst).desc = d;
            (*dst).n_desc = n;
            (*dst).metadata_json = c_strdup("{}");
        }
        Arg::Group {
            schema,
            type_desc,
            metavar,
            desc,
            group_opt,
            entries,
            constraints,
            ..
        } => {
            (*dst).kind = ManifestArgKind::Grp;
            (*dst).schema = nullable_strdup(schema.as_deref());
            (*dst).type_desc = nullable_strdup(type_desc.as_deref());
            (*dst).metavar = nullable_strdup(metavar.as_deref());
            let (d, n) = populate_str_vec(desc);
            (*dst).desc = d;
            (*dst).n_desc = n;
            if let Some(g) = group_opt {
                (*dst).grp_short = g
                    .short_opt
                    .as_ref()
                    .and_then(|s| s.as_bytes().first().copied())
                    .map(|b| b as c_char)
                    .unwrap_or(0);
                (*dst).grp_long = nullable_strdup(g.long_opt.as_deref());
            }
            if !entries.is_empty() {
                (*dst).n_entries = entries.len();
                (*dst).entries = libc::calloc(
                    entries.len(),
                    std::mem::size_of::<ManifestGrpEntry>(),
                ) as *mut ManifestGrpEntry;
                for (i, ge) in entries.iter().enumerate() {
                    let ge_dst = &mut *(*dst).entries.add(i);
                    ge_dst.key = c_strdup(&ge.key);
                    let sub_arg = libc::calloc(1, std::mem::size_of::<ManifestArg>())
                        as *mut ManifestArg;
                    populate_arg(sub_arg, &ge.arg);
                    ge_dst.arg = sub_arg;
                }
            }
            let (cs, nc) = populate_constraints(constraints);
            (*dst).constraints = cs;
            (*dst).n_constraints = nc;
            (*dst).metadata_json = c_strdup("{}");
        }
    }
}

unsafe fn populate_return(dst: *mut ManifestReturn, src: &morloc_manifest::Return) {
    (*dst).schema = c_strdup(&src.schema);
    (*dst).type_desc = c_strdup(&src.type_desc);
    let (d, n) = populate_str_vec(&src.desc);
    (*dst).desc = d;
    (*dst).n_desc = n;
    let (cs, nc) = populate_constraints(&src.constraints);
    (*dst).constraints = cs;
    (*dst).n_constraints = nc;
    (*dst).metadata_json = populate_metadata(&src.metadata);
}

unsafe fn populate_command(dst: *mut ManifestCommand, src: &morloc_manifest::Command) -> Result<(), MorlocError> {
    (*dst).name = c_strdup(&src.name);
    (*dst).is_pure = src.is_pure();
    (*dst).mid = src.mid;
    (*dst).pool_index = src.pool_index;
    if !src.needed_pools.is_empty() {
        (*dst).n_needed_pools = src.needed_pools.len();
        (*dst).needed_pools = libc::calloc(
            src.needed_pools.len(),
            std::mem::size_of::<usize>(),
        ) as *mut usize;
        for (i, p) in src.needed_pools.iter().enumerate() {
            *(*dst).needed_pools.add(i) = *p;
        }
    }
    let (d, n) = populate_str_vec(&src.desc);
    (*dst).desc = d;
    (*dst).n_desc = n;

    if !src.args.is_empty() {
        (*dst).n_args = src.args.len();
        (*dst).args = libc::calloc(
            src.args.len(),
            std::mem::size_of::<ManifestArg>(),
        ) as *mut ManifestArg;
        for (i, a) in src.args.iter().enumerate() {
            populate_arg((*dst).args.add(i), a);
        }
    }

    populate_return(&mut (*dst).ret, &src.ret);

    let (cs, nc) = populate_constraints(&src.constraints);
    (*dst).constraints = cs;
    (*dst).n_constraints = nc;

    (*dst).metadata_json = populate_metadata(&src.metadata);

    if src.is_pure() {
        if let Some(expr_val) = &src.expr {
            match build_expr(expr_val) {
                Ok(e) => (*dst).expr = e,
                Err(e) => return Err(e),
            }
        }
    }

    (*dst).group = match &src.group {
        Some(g) => c_strdup(g),
        None => ptr::null_mut(),
    };

    Ok(())
}

unsafe fn populate_pool(dst: *mut ManifestPool, src: &morloc_manifest::Pool) {
    (*dst).lang = c_strdup(&src.lang);
    let n = src.exec.len();
    (*dst).exec = libc::calloc(n + 1, std::mem::size_of::<*mut c_char>()) as *mut *mut c_char;
    for (i, e) in src.exec.iter().enumerate() {
        *(*dst).exec.add(i) = c_strdup(e);
    }
    *(*dst).exec.add(n) = ptr::null_mut();
    (*dst).socket = c_strdup(&src.socket);
    (*dst).metadata_json = populate_metadata(&src.metadata);
}

unsafe fn populate_cmd_group(dst: *mut ManifestCmdGroup, src: &morloc_manifest::CmdGroup) {
    (*dst).name = c_strdup(&src.name);
    let (d, n) = populate_str_vec(&src.desc);
    (*dst).desc = d;
    (*dst).n_desc = n;
    (*dst).metadata_json = populate_metadata(&src.metadata);
}

unsafe fn populate_service(dst: *mut ManifestService, src: &morloc_manifest::Service) {
    (*dst).stype = nullable_strdup(src.service_type.as_deref());
    (*dst).host = nullable_strdup(src.host.as_deref());
    (*dst).port = src.port.unwrap_or(0);
    (*dst).socket = nullable_strdup(src.socket.as_deref());
    (*dst).metadata_json = populate_metadata(&src.metadata);
}

// -- parse_manifest -----------------------------------------------------------
//
// Reads a manifest JSON string, parses it via the canonical
// morloc-manifest crate (which performs the version staleness check),
// and converts the resulting Rust struct into owned C structs for
// daemon-side consumers. There is no separate JSON walker here -- the
// shape and validation rules live in one place (the morloc-manifest
// crate).

#[no_mangle]
pub unsafe extern "C" fn parse_manifest(
    text: *const c_char,
    errmsg: *mut *mut c_char,
) -> *mut Manifest {
    clear_errmsg(errmsg);
    let s = CStr::from_ptr(text).to_string_lossy();

    let parsed = match morloc_manifest::parse_manifest(&s) {
        Ok(p) => p,
        Err(e) => {
            set_errmsg(errmsg, &MorlocError::Other(e));
            return ptr::null_mut();
        }
    };

    let m = libc::calloc(1, std::mem::size_of::<Manifest>()) as *mut Manifest;
    (*m).name = c_strdup(&parsed.name);

    // build sub-object
    (*m).build.path = c_strdup(&parsed.build.path);
    (*m).build.time = parsed.build.time;
    (*m).build.morloc_version = c_strdup(&parsed.build.morloc_version);

    // pools
    if !parsed.pools.is_empty() {
        (*m).n_pools = parsed.pools.len();
        (*m).pools = libc::calloc(
            parsed.pools.len(),
            std::mem::size_of::<ManifestPool>(),
        ) as *mut ManifestPool;
        for (i, p) in parsed.pools.iter().enumerate() {
            populate_pool((*m).pools.add(i), p);
        }
    }

    // commands
    if !parsed.commands.is_empty() {
        (*m).n_commands = parsed.commands.len();
        (*m).commands = libc::calloc(
            parsed.commands.len(),
            std::mem::size_of::<ManifestCommand>(),
        ) as *mut ManifestCommand;
        for (i, c) in parsed.commands.iter().enumerate() {
            if let Err(e) = populate_command((*m).commands.add(i), c) {
                set_errmsg(errmsg, &e);
                return ptr::null_mut();
            }
        }
    }

    // groups
    if !parsed.groups.is_empty() {
        (*m).n_groups = parsed.groups.len();
        (*m).groups = libc::calloc(
            parsed.groups.len(),
            std::mem::size_of::<ManifestCmdGroup>(),
        ) as *mut ManifestCmdGroup;
        for (i, g) in parsed.groups.iter().enumerate() {
            populate_cmd_group((*m).groups.add(i), g);
        }
    }

    // service
    if let Some(svc) = parsed.service {
        (*m).service =
            libc::calloc(1, std::mem::size_of::<ManifestService>()) as *mut ManifestService;
        populate_service((*m).service, &svc);
    }

    (*m).metadata_json = populate_metadata(&parsed.metadata);

    m
}

// -- read_manifest ------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn read_manifest(
    path: *const c_char,
    errmsg: *mut *mut c_char,
) -> *mut Manifest {
    clear_errmsg(errmsg);
    let path_str = CStr::from_ptr(path).to_string_lossy();
    match std::fs::read_to_string(path_str.as_ref()) {
        Ok(text) => {
            let c_text = CString::new(text).unwrap_or_default();
            parse_manifest(c_text.as_ptr(), errmsg)
        }
        Err(e) => {
            set_errmsg(errmsg, &MorlocError::Io(e));
            ptr::null_mut()
        }
    }
}

// -- free_manifest ------------------------------------------------------------
//
// Walks the v2 C structs and releases every owned C string + sub-allocation.
// Helper functions mirror the populate_* helpers above for symmetry.

unsafe fn free_str_array(arr: *mut *mut c_char) {
    if arr.is_null() {
        return;
    }
    let mut j = 0;
    while !(*arr.add(j)).is_null() {
        libc::free(*arr.add(j) as *mut c_void);
        j += 1;
    }
    libc::free(arr as *mut c_void);
}

unsafe fn free_constraints(ptr: *mut ManifestConstraint, n: usize) {
    if ptr.is_null() {
        return;
    }
    for i in 0..n {
        let c = &*ptr.add(i);
        if !c.ctype.is_null() {
            libc::free(c.ctype as *mut c_void);
        }
        if !c.value_json.is_null() {
            libc::free(c.value_json as *mut c_void);
        }
    }
    libc::free(ptr as *mut c_void);
}

unsafe fn free_arg(arg: &ManifestArg) {
    if !arg.schema.is_null() {
        libc::free(arg.schema as *mut c_void);
    }
    if !arg.type_desc.is_null() {
        libc::free(arg.type_desc as *mut c_void);
    }
    if !arg.metavar.is_null() {
        libc::free(arg.metavar as *mut c_void);
    }
    if !arg.long_opt.is_null() {
        libc::free(arg.long_opt as *mut c_void);
    }
    if !arg.long_rev.is_null() {
        libc::free(arg.long_rev as *mut c_void);
    }
    if !arg.default_val.is_null() {
        libc::free(arg.default_val as *mut c_void);
    }
    free_str_array(arg.desc);
    free_constraints(arg.constraints, arg.n_constraints);
    if !arg.grp_long.is_null() {
        libc::free(arg.grp_long as *mut c_void);
    }
    if !arg.entries.is_null() {
        for i in 0..arg.n_entries {
            let ge = &*arg.entries.add(i);
            if !ge.key.is_null() {
                libc::free(ge.key as *mut c_void);
            }
            if !ge.arg.is_null() {
                free_arg(&*ge.arg);
                libc::free(ge.arg as *mut c_void);
            }
        }
        libc::free(arg.entries as *mut c_void);
    }
    if !arg.metadata_json.is_null() {
        libc::free(arg.metadata_json as *mut c_void);
    }
}

unsafe fn free_return(ret: &ManifestReturn) {
    if !ret.schema.is_null() {
        libc::free(ret.schema as *mut c_void);
    }
    if !ret.type_desc.is_null() {
        libc::free(ret.type_desc as *mut c_void);
    }
    free_str_array(ret.desc);
    free_constraints(ret.constraints, ret.n_constraints);
    if !ret.metadata_json.is_null() {
        libc::free(ret.metadata_json as *mut c_void);
    }
}

#[no_mangle]
pub unsafe extern "C" fn free_manifest(manifest: *mut Manifest) {
    if manifest.is_null() {
        return;
    }
    let m = &*manifest;
    if !m.name.is_null() {
        libc::free(m.name as *mut c_void);
    }
    // build sub-object
    if !m.build.path.is_null() {
        libc::free(m.build.path as *mut c_void);
    }
    if !m.build.morloc_version.is_null() {
        libc::free(m.build.morloc_version as *mut c_void);
    }
    // pools
    for i in 0..m.n_pools {
        let pool = &*m.pools.add(i);
        if !pool.lang.is_null() {
            libc::free(pool.lang as *mut c_void);
        }
        free_str_array(pool.exec);
        if !pool.socket.is_null() {
            libc::free(pool.socket as *mut c_void);
        }
        if !pool.metadata_json.is_null() {
            libc::free(pool.metadata_json as *mut c_void);
        }
    }
    if !m.pools.is_null() {
        libc::free(m.pools as *mut c_void);
    }
    // commands
    for i in 0..m.n_commands {
        let cmd = &*m.commands.add(i);
        if !cmd.name.is_null() {
            libc::free(cmd.name as *mut c_void);
        }
        if !cmd.needed_pools.is_null() {
            libc::free(cmd.needed_pools as *mut c_void);
        }
        free_str_array(cmd.desc);
        for j in 0..cmd.n_args {
            free_arg(&*cmd.args.add(j));
        }
        if !cmd.args.is_null() {
            libc::free(cmd.args as *mut c_void);
        }
        free_return(&cmd.ret);
        free_constraints(cmd.constraints, cmd.n_constraints);
        if !cmd.group.is_null() {
            libc::free(cmd.group as *mut c_void);
        }
        if !cmd.metadata_json.is_null() {
            libc::free(cmd.metadata_json as *mut c_void);
        }
        // Note: cmd.expr is owned by the C side and freed by its own
        // free function in eval_ffi.rs (not in scope here).
    }
    if !m.commands.is_null() {
        libc::free(m.commands as *mut c_void);
    }
    // groups
    for i in 0..m.n_groups {
        let g = &*m.groups.add(i);
        if !g.name.is_null() {
            libc::free(g.name as *mut c_void);
        }
        free_str_array(g.desc);
        if !g.metadata_json.is_null() {
            libc::free(g.metadata_json as *mut c_void);
        }
    }
    if !m.groups.is_null() {
        libc::free(m.groups as *mut c_void);
    }
    // service
    if !m.service.is_null() {
        let svc = &*m.service;
        if !svc.stype.is_null() {
            libc::free(svc.stype as *mut c_void);
        }
        if !svc.host.is_null() {
            libc::free(svc.host as *mut c_void);
        }
        if !svc.socket.is_null() {
            libc::free(svc.socket as *mut c_void);
        }
        if !svc.metadata_json.is_null() {
            libc::free(svc.metadata_json as *mut c_void);
        }
        libc::free(m.service as *mut c_void);
    }
    if !m.metadata_json.is_null() {
        libc::free(m.metadata_json as *mut c_void);
    }
    libc::free(manifest as *mut c_void);
}

// -- manifest_to_discovery_json -----------------------------------------------
//
// Emits a v2-shape JSON describing the manifest's commands. Used by the
// daemon/router code for discovery RPC. The output mirrors the morloc
// compiler's manifest format closely (no v1 legacy field names).

#[no_mangle]
pub unsafe extern "C" fn manifest_to_discovery_json(manifest: *const Manifest) -> *mut c_char {
    if manifest.is_null() {
        return ptr::null_mut();
    }
    let m = &*manifest;

    extern "C" {
        fn json_buf_new() -> *mut c_void;
        fn json_buf_finish(jb: *mut c_void) -> *mut c_char;
        fn json_write_obj_start(jb: *mut c_void);
        fn json_write_obj_end(jb: *mut c_void);
        fn json_write_arr_start(jb: *mut c_void);
        fn json_write_arr_end(jb: *mut c_void);
        fn json_write_key(jb: *mut c_void, key: *const c_char);
        fn json_write_string(jb: *mut c_void, val: *const c_char);
    }

    let jb = json_buf_new();
    json_write_obj_start(jb);

    let name_key = b"name\0".as_ptr() as *const c_char;
    let type_key = b"type\0".as_ptr() as *const c_char;
    let kind_key = b"kind\0".as_ptr() as *const c_char;
    let schema_key = b"schema\0".as_ptr() as *const c_char;

    json_write_key(jb, name_key);
    json_write_string(
        jb,
        if m.name.is_null() {
            b"unknown\0".as_ptr() as *const c_char
        } else {
            m.name
        },
    );

    // Surface the morloc compiler version that built this manifest.
    if !m.build.morloc_version.is_null() {
        let mv_key = b"morloc_version\0".as_ptr() as *const c_char;
        json_write_key(jb, mv_key);
        json_write_string(jb, m.build.morloc_version);
    }

    let commands_key = b"commands\0".as_ptr() as *const c_char;
    json_write_key(jb, commands_key);
    json_write_arr_start(jb);

    for i in 0..m.n_commands {
        let cmd = &*m.commands.add(i);
        json_write_obj_start(jb);

        json_write_key(jb, name_key);
        json_write_string(jb, cmd.name);

        json_write_key(jb, type_key);
        json_write_string(
            jb,
            if cmd.is_pure {
                b"pure\0".as_ptr() as *const c_char
            } else {
                b"remote\0".as_ptr() as *const c_char
            },
        );

        // Return descriptor (nested return object).
        if !cmd.ret.type_desc.is_null() || !cmd.ret.schema.is_null() {
            let ret_key = b"return\0".as_ptr() as *const c_char;
            json_write_key(jb, ret_key);
            json_write_obj_start(jb);
            if !cmd.ret.type_desc.is_null() {
                json_write_key(jb, type_key);
                json_write_string(jb, cmd.ret.type_desc);
            }
            if !cmd.ret.schema.is_null() {
                json_write_key(jb, schema_key);
                json_write_string(jb, cmd.ret.schema);
            }
            json_write_obj_end(jb);
        }

        // Args. Each arg's schema (if any) is on the arg itself; no
        // parallel array, no flag-skipping bug.
        let args_key = b"args\0".as_ptr() as *const c_char;
        json_write_key(jb, args_key);
        json_write_arr_start(jb);
        for a in 0..cmd.n_args {
            let arg = &*cmd.args.add(a);
            json_write_obj_start(jb);

            json_write_key(jb, kind_key);
            match arg.kind {
                ManifestArgKind::Pos => json_write_string(jb, b"pos\0".as_ptr() as *const c_char),
                ManifestArgKind::Opt => json_write_string(jb, b"opt\0".as_ptr() as *const c_char),
                ManifestArgKind::Flag => json_write_string(jb, b"flag\0".as_ptr() as *const c_char),
                ManifestArgKind::Grp => json_write_string(jb, b"grp\0".as_ptr() as *const c_char),
            }

            if !arg.metavar.is_null() {
                json_write_key(jb, b"metavar\0".as_ptr() as *const c_char);
                json_write_string(jb, arg.metavar);
            }
            if !arg.type_desc.is_null() {
                json_write_key(jb, type_key);
                json_write_string(jb, arg.type_desc);
            }
            if !arg.schema.is_null() {
                json_write_key(jb, schema_key);
                json_write_string(jb, arg.schema);
            }
            if !arg.default_val.is_null() {
                json_write_key(jb, b"default\0".as_ptr() as *const c_char);
                json_write_string(jb, arg.default_val);
            }
            if !arg.long_opt.is_null() {
                json_write_key(jb, b"long\0".as_ptr() as *const c_char);
                json_write_string(jb, arg.long_opt);
            }
            if arg.short_opt != 0 {
                let short_str = [arg.short_opt as u8, 0];
                json_write_key(jb, b"short\0".as_ptr() as *const c_char);
                json_write_string(jb, short_str.as_ptr() as *const c_char);
            }
            if arg.n_desc > 0 && !arg.desc.is_null() && !(*arg.desc).is_null() {
                let first = *arg.desc;
                if *first != 0 {
                    let desc_key = b"desc\0".as_ptr() as *const c_char;
                    json_write_key(jb, desc_key);
                    json_write_string(jb, first);
                }
            }

            json_write_obj_end(jb);
        }
        json_write_arr_end(jb);

        if cmd.n_desc > 0 && !cmd.desc.is_null() && !(*cmd.desc).is_null() {
            let first = *cmd.desc;
            if *first != 0 {
                json_write_key(jb, b"desc\0".as_ptr() as *const c_char);
                json_write_string(jb, first);
            }
        }

        if !cmd.group.is_null() {
            json_write_key(jb, b"group\0".as_ptr() as *const c_char);
            json_write_string(jb, cmd.group);
        }

        json_write_obj_end(jb);
    }

    json_write_arr_end(jb);

    if m.n_groups > 0 {
        let groups_key = b"groups\0".as_ptr() as *const c_char;
        json_write_key(jb, groups_key);
        json_write_arr_start(jb);
        for i in 0..m.n_groups {
            let g = &*m.groups.add(i);
            json_write_obj_start(jb);
            json_write_key(jb, name_key);
            json_write_string(jb, g.name);
            if g.n_desc > 0 && !g.desc.is_null() && !(*g.desc).is_null() {
                json_write_key(jb, b"desc\0".as_ptr() as *const c_char);
                json_write_string(jb, *g.desc);
            }
            json_write_obj_end(jb);
        }
        json_write_arr_end(jb);
    }

    json_write_obj_end(jb);
    json_buf_finish(jb)
}
