//! C ABI wrappers for manifest parsing and discovery JSON.
//! Replaces manifest.c. Uses serde_json instead of hand-rolled JSON parser.

use std::ffi::{c_char, c_void, CStr, CString};
use std::ptr;

use crate::cschema::CSchema;
use crate::error::{clear_errmsg, set_errmsg, MorlocError};

// ── C-compatible types matching eval.h ───────────────────────────────────────

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
pub union ExprUnion {
    pub app_expr: *mut MorlocAppExpression,
    pub lam_expr: *mut MorlocLamExpression,
    pub bnd_expr: *mut c_char,
    pub interpolation: *mut *mut c_char,
    pub pattern_expr: *mut MorlocPattern,
    pub data_expr: *mut MorlocData,
    pub unary_expr: *mut MorlocExpression,
}

#[repr(C)]
pub struct MorlocExpression {
    pub etype: MorlocExpressionType,
    pub schema: *mut CSchema,
    pub expr: ExprUnion,
}

// ── C-compatible types matching manifest.h ───────────────────────────────────

#[repr(C)]
pub struct ManifestPool {
    pub lang: *mut c_char,
    pub exec: *mut *mut c_char, // NULL-terminated
    pub socket: *mut c_char,
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
    pub desc: *mut *mut c_char,
    pub metavar: *mut c_char,
    pub type_desc: *mut c_char,
    pub quoted: bool,
    pub short_opt: c_char,
    pub long_opt: *mut c_char,
    pub long_rev: *mut c_char,
    pub default_val: *mut c_char,
    pub grp_short: c_char,
    pub grp_long: *mut c_char,
    pub entries: *mut ManifestGrpEntry,
    pub n_entries: usize,
}

#[repr(C)]
pub struct ManifestCmdGroup {
    pub name: *mut c_char,
    pub desc: *mut *mut c_char,
}

#[repr(C)]
pub struct ManifestCommand {
    pub name: *mut c_char,
    pub is_pure: bool,
    pub mid: u32,
    pub pool_index: usize,
    pub needed_pools: *mut usize,
    pub n_needed_pools: usize,
    pub arg_schemas: *mut *mut c_char,
    pub return_schema: *mut c_char,
    pub desc: *mut *mut c_char,
    pub return_type: *mut c_char,
    pub return_desc: *mut *mut c_char,
    pub args: *mut ManifestArg,
    pub n_args: usize,
    pub expr: *mut MorlocExpression,
    pub group: *mut c_char,
}

#[repr(C)]
pub struct ManifestService {
    pub stype: *mut c_char,
    pub host: *mut c_char,
    pub port: i32,
    pub socket: *mut c_char,
}

#[repr(C)]
pub struct Manifest {
    pub version: i32,
    pub name: *mut c_char,
    pub build_dir: *mut c_char,
    pub pools: *mut ManifestPool,
    pub n_pools: usize,
    pub commands: *mut ManifestCommand,
    pub n_commands: usize,
    pub groups: *mut ManifestCmdGroup,
    pub n_groups: usize,
    pub service: *mut ManifestService,
}

// ── Helpers ──────────────────────────────────────────────────────────────────

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

unsafe fn json_str_array(arr: &[serde_json::Value]) -> *mut *mut c_char {
    let out = libc::calloc(arr.len() + 1, std::mem::size_of::<*mut c_char>()) as *mut *mut c_char;
    for (i, v) in arr.iter().enumerate() {
        let s = v.as_str().unwrap_or("");
        *out.add(i) = c_strdup(s);
    }
    *out.add(arr.len()) = ptr::null_mut();
    out
}

unsafe fn empty_str_array() -> *mut *mut c_char {
    let out = libc::calloc(1, std::mem::size_of::<*mut c_char>()) as *mut *mut c_char;
    *out = ptr::null_mut();
    out
}

// ── Expression builder (using serde_json::Value) ─────────────────────────────

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

        "show" | "read" => {
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
            (*expr).etype = if tag == "show" { MorlocExpressionType::Show } else { MorlocExpressionType::Read };
            (*expr).schema = schema;
            (*expr).expr.unary_expr = child;
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

// ── build_manifest_expr ──────────────────────────────────────────────────────

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

// ── Manifest argument reader ─────────────────────────────────────────────────

unsafe fn read_arg(ja: &serde_json::Value) -> Result<ManifestArg, MorlocError> {
    let kind = ja.get("kind").and_then(|v| v.as_str()).ok_or_else(|| MorlocError::Other("Argument missing 'kind' field".into()))?;

    let mut arg: ManifestArg = std::mem::zeroed();
    arg.desc = match ja.get("desc").and_then(|v| v.as_array()) {
        Some(arr) => json_str_array(arr),
        None => empty_str_array(),
    };

    match kind {
        "pos" => {
            arg.kind = ManifestArgKind::Pos;
            arg.metavar = nullable_strdup(ja.get("metavar").and_then(|v| v.as_str()));
            arg.type_desc = nullable_strdup(ja.get("type_desc").and_then(|v| v.as_str()));
            arg.quoted = ja.get("quoted").and_then(|v| v.as_bool()).unwrap_or(false);
        }
        "opt" => {
            arg.kind = ManifestArgKind::Opt;
            arg.metavar = nullable_strdup(ja.get("metavar").and_then(|v| v.as_str()));
            arg.type_desc = nullable_strdup(ja.get("type_desc").and_then(|v| v.as_str()));
            arg.quoted = ja.get("quoted").and_then(|v| v.as_bool()).unwrap_or(false);
            let s = ja.get("short").and_then(|v| v.as_str()).unwrap_or("");
            arg.short_opt = if !s.is_empty() { s.as_bytes()[0] as c_char } else { 0 };
            arg.long_opt = nullable_strdup(ja.get("long").and_then(|v| v.as_str()));
            arg.default_val = nullable_strdup(ja.get("default").and_then(|v| v.as_str()));
        }
        "flag" => {
            arg.kind = ManifestArgKind::Flag;
            let s = ja.get("short").and_then(|v| v.as_str()).unwrap_or("");
            arg.short_opt = if !s.is_empty() { s.as_bytes()[0] as c_char } else { 0 };
            arg.long_opt = nullable_strdup(ja.get("long").and_then(|v| v.as_str()));
            arg.long_rev = nullable_strdup(ja.get("long_rev").and_then(|v| v.as_str()));
            arg.default_val = nullable_strdup(ja.get("default").and_then(|v| v.as_str()));
        }
        "grp" => {
            arg.kind = ManifestArgKind::Grp;
            arg.metavar = nullable_strdup(ja.get("metavar").and_then(|v| v.as_str()));
            if let Some(gopt) = ja.get("group_opt") {
                if !gopt.is_null() {
                    let gs = gopt.get("short").and_then(|v| v.as_str()).unwrap_or("");
                    arg.grp_short = if !gs.is_empty() { gs.as_bytes()[0] as c_char } else { 0 };
                    arg.grp_long = nullable_strdup(gopt.get("long").and_then(|v| v.as_str()));
                }
            }
            if let Some(entries) = ja.get("entries").and_then(|v| v.as_array()) {
                arg.n_entries = entries.len();
                arg.entries = libc::calloc(entries.len(), std::mem::size_of::<ManifestGrpEntry>()) as *mut ManifestGrpEntry;
                for (i, entry) in entries.iter().enumerate() {
                    let ge = &mut *arg.entries.add(i);
                    ge.key = c_strdup(entry.get("key").and_then(|v| v.as_str()).unwrap_or(""));
                    let sub = libc::calloc(1, std::mem::size_of::<ManifestArg>()) as *mut ManifestArg;
                    *sub = read_arg(entry.get("arg").unwrap_or(&serde_json::Value::Null))?;
                    ge.arg = sub;
                }
            }
        }
        _ => return Err(MorlocError::Other(format!("Unknown arg kind: {}", kind))),
    }

    Ok(arg)
}

// ── parse_manifest ───────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn parse_manifest(
    text: *const c_char,
    errmsg: *mut *mut c_char,
) -> *mut Manifest {
    clear_errmsg(errmsg);
    let s = CStr::from_ptr(text).to_string_lossy();

    let root: serde_json::Value = match serde_json::from_str(&s) {
        Ok(v) => v,
        Err(e) => {
            set_errmsg(errmsg, &MorlocError::Other(format!("Failed to parse manifest JSON: {}", e)));
            return ptr::null_mut();
        }
    };

    let m = libc::calloc(1, std::mem::size_of::<Manifest>()) as *mut Manifest;
    (*m).version = root.get("version").and_then(|v| v.as_f64()).unwrap_or(0.0) as i32;
    (*m).name = nullable_strdup(root.get("name").and_then(|v| v.as_str()));
    (*m).build_dir = nullable_strdup(root.get("build_dir").and_then(|v| v.as_str()));

    // Pools
    if let Some(pools) = root.get("pools").and_then(|v| v.as_array()) {
        (*m).n_pools = pools.len();
        (*m).pools = libc::calloc(pools.len(), std::mem::size_of::<ManifestPool>()) as *mut ManifestPool;
        for (i, jp) in pools.iter().enumerate() {
            let pool = &mut *(*m).pools.add(i);
            pool.lang = c_strdup(jp.get("lang").and_then(|v| v.as_str()).unwrap_or(""));
            pool.exec = match jp.get("exec").and_then(|v| v.as_array()) {
                Some(arr) => json_str_array(arr),
                None => empty_str_array(),
            };
            pool.socket = c_strdup(jp.get("socket").and_then(|v| v.as_str()).unwrap_or(""));
        }
    }

    // Commands
    if let Some(cmds) = root.get("commands").and_then(|v| v.as_array()) {
        (*m).n_commands = cmds.len();
        (*m).commands = libc::calloc(cmds.len(), std::mem::size_of::<ManifestCommand>()) as *mut ManifestCommand;
        for (i, jc) in cmds.iter().enumerate() {
            let cmd = &mut *(*m).commands.add(i);
            cmd.name = c_strdup(jc.get("name").and_then(|v| v.as_str()).unwrap_or(""));
            let ctype = jc.get("type").and_then(|v| v.as_str()).unwrap_or("");
            cmd.is_pure = ctype == "pure";

            if !cmd.is_pure {
                cmd.mid = jc.get("mid").and_then(|v| v.as_f64()).unwrap_or(0.0) as u32;
                cmd.pool_index = jc.get("pool").and_then(|v| v.as_f64()).unwrap_or(0.0) as usize;
                if let Some(np) = jc.get("needed_pools").and_then(|v| v.as_array()) {
                    cmd.n_needed_pools = np.len();
                    cmd.needed_pools = libc::calloc(np.len(), std::mem::size_of::<usize>()) as *mut usize;
                    for (j, v) in np.iter().enumerate() {
                        *cmd.needed_pools.add(j) = v.as_f64().unwrap_or(0.0) as usize;
                    }
                }
            }

            cmd.arg_schemas = match jc.get("arg_schemas").and_then(|v| v.as_array()) {
                Some(arr) => json_str_array(arr),
                None => empty_str_array(),
            };
            cmd.return_schema = c_strdup(jc.get("return_schema").and_then(|v| v.as_str()).unwrap_or(""));
            cmd.desc = match jc.get("desc").and_then(|v| v.as_array()) {
                Some(arr) => json_str_array(arr),
                None => empty_str_array(),
            };
            cmd.return_type = c_strdup(jc.get("return_type").and_then(|v| v.as_str()).unwrap_or(""));
            cmd.return_desc = match jc.get("return_desc").and_then(|v| v.as_array()) {
                Some(arr) => json_str_array(arr),
                None => empty_str_array(),
            };

            // Args
            if let Some(jargs) = jc.get("args").and_then(|v| v.as_array()) {
                cmd.n_args = jargs.len();
                cmd.args = libc::calloc(jargs.len(), std::mem::size_of::<ManifestArg>()) as *mut ManifestArg;
                for (j, ja) in jargs.iter().enumerate() {
                    match read_arg(ja) {
                        Ok(arg) => *cmd.args.add(j) = arg,
                        Err(e) => {
                            set_errmsg(errmsg, &e);
                            return ptr::null_mut();
                        }
                    }
                }
            }

            if cmd.is_pure {
                if let Some(expr_val) = jc.get("expr") {
                    match build_expr(expr_val) {
                        Ok(expr) => cmd.expr = expr,
                        Err(e) => {
                            set_errmsg(errmsg, &e);
                            return ptr::null_mut();
                        }
                    }
                }
            }

            cmd.group = nullable_strdup(jc.get("group").and_then(|v| v.as_str()));
        }
    }

    // Groups
    if let Some(groups) = root.get("groups").and_then(|v| v.as_array()) {
        (*m).n_groups = groups.len();
        (*m).groups = libc::calloc(groups.len(), std::mem::size_of::<ManifestCmdGroup>()) as *mut ManifestCmdGroup;
        for (i, jg) in groups.iter().enumerate() {
            let g = &mut *(*m).groups.add(i);
            g.name = c_strdup(jg.get("name").and_then(|v| v.as_str()).unwrap_or(""));
            g.desc = match jg.get("desc").and_then(|v| v.as_array()) {
                Some(arr) => json_str_array(arr),
                None => empty_str_array(),
            };
        }
    }

    m
}

// ── read_manifest ────────────────────────────────────────────────────────────

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

// ── free_manifest ────────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn free_manifest(manifest: *mut Manifest) {
    if manifest.is_null() { return; }
    let m = &*manifest;
    if !m.name.is_null() { libc::free(m.name as *mut c_void); }
    if !m.build_dir.is_null() { libc::free(m.build_dir as *mut c_void); }
    for i in 0..m.n_pools {
        let pool = &*m.pools.add(i);
        if !pool.lang.is_null() { libc::free(pool.lang as *mut c_void); }
        if !pool.exec.is_null() {
            let mut j = 0;
            while !(*pool.exec.add(j)).is_null() {
                libc::free(*pool.exec.add(j) as *mut c_void);
                j += 1;
            }
            libc::free(pool.exec as *mut c_void);
        }
        if !pool.socket.is_null() { libc::free(pool.socket as *mut c_void); }
    }
    if !m.pools.is_null() { libc::free(m.pools as *mut c_void); }
    if !m.commands.is_null() { libc::free(m.commands as *mut c_void); }
    for i in 0..m.n_groups {
        let g = &*m.groups.add(i);
        if !g.name.is_null() { libc::free(g.name as *mut c_void); }
        if !g.desc.is_null() {
            let mut j = 0;
            while !(*g.desc.add(j)).is_null() {
                libc::free(*g.desc.add(j) as *mut c_void);
                j += 1;
            }
            libc::free(g.desc as *mut c_void);
        }
    }
    if !m.groups.is_null() { libc::free(m.groups as *mut c_void); }
    libc::free(manifest as *mut c_void);
}

// ── manifest_to_discovery_json ───────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn manifest_to_discovery_json(
    manifest: *const Manifest,
) -> *mut c_char {
    if manifest.is_null() { return ptr::null_mut(); }
    let m = &*manifest;

    // Use the json_buf API (provided by json_ffi.rs)
    extern "C" {
        fn json_buf_new() -> *mut c_void;
        fn json_buf_finish(jb: *mut c_void) -> *mut c_char;
        fn json_write_obj_start(jb: *mut c_void);
        fn json_write_obj_end(jb: *mut c_void);
        fn json_write_arr_start(jb: *mut c_void);
        fn json_write_arr_end(jb: *mut c_void);
        fn json_write_key(jb: *mut c_void, key: *const c_char);
        fn json_write_string(jb: *mut c_void, val: *const c_char);
        fn json_write_int(jb: *mut c_void, val: i64);
    }

    let jb = json_buf_new();

    json_write_obj_start(jb);

    let name_key = b"name\0".as_ptr() as *const c_char;
    json_write_key(jb, name_key);
    json_write_string(jb, if m.name.is_null() { b"unknown\0".as_ptr() as *const c_char } else { m.name });

    let version_key = b"version\0".as_ptr() as *const c_char;
    json_write_key(jb, version_key);
    json_write_int(jb, m.version as i64);

    let commands_key = b"commands\0".as_ptr() as *const c_char;
    json_write_key(jb, commands_key);
    json_write_arr_start(jb);

    for i in 0..m.n_commands {
        let cmd = &*m.commands.add(i);
        json_write_obj_start(jb);

        json_write_key(jb, name_key);
        json_write_string(jb, cmd.name);

        let type_key = b"type\0".as_ptr() as *const c_char;
        json_write_key(jb, type_key);
        json_write_string(jb, if cmd.is_pure { b"pure\0".as_ptr() as *const c_char } else { b"remote\0".as_ptr() as *const c_char });

        let rt_key = b"return_type\0".as_ptr() as *const c_char;
        json_write_key(jb, rt_key);
        json_write_string(jb, cmd.return_type);

        let rs_key = b"return_schema\0".as_ptr() as *const c_char;
        json_write_key(jb, rs_key);
        json_write_string(jb, cmd.return_schema);

        // Args
        let args_key = b"args\0".as_ptr() as *const c_char;
        json_write_key(jb, args_key);
        json_write_arr_start(jb);
        for a in 0..cmd.n_args {
            let arg = &*cmd.args.add(a);
            json_write_obj_start(jb);

            let kind_key = b"kind\0".as_ptr() as *const c_char;
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
            if (arg.kind == ManifestArgKind::Pos || arg.kind == ManifestArgKind::Opt)
                && !cmd.arg_schemas.is_null() && !(*cmd.arg_schemas.add(a)).is_null() {
                let schema_key = b"schema\0".as_ptr() as *const c_char;
                json_write_key(jb, schema_key);
                json_write_string(jb, *cmd.arg_schemas.add(a));
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
            if !arg.desc.is_null() && !(*arg.desc).is_null() {
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

        if !cmd.desc.is_null() && !(*cmd.desc).is_null() {
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
            if !g.desc.is_null() && !(*g.desc).is_null() {
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
