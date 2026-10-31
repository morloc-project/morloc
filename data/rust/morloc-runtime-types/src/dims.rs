//! The dimension relations a command's input must meet (DIM-3, the data
//! level): read from the command's `dims` constraint, checked on the
//! decoded arguments before the command runs.

use crate::schema::{Schema, SerialType};
use crate::shm_types::{Array, RelPtr, RELNULL};

/// The magic number heading a table's Arrow block, and where in the block
/// its row count is; the runtime asserts both against its block header.
pub const TABLE_MAGIC: u32 = 0xA770DA7A;
pub const TABLE_ROWS_AT: usize = 16;

/// Resolves a relative pointer to the address it names.
pub type Resolve<'a> = &'a dyn Fn(RelPtr) -> Result<*const u8, String>;

/// Names argument `i` as a user names it, given the record field its path
/// starts at, and returns the leading part of a location the name says.
pub type Label<'a> = &'a dyn Fn(usize, Option<&str>) -> (String, String);

enum Step {
    Each,
    Field(usize),
    Some,
}

struct Bind {
    var: usize,
    arg: usize,
    path: Vec<Step>,
    value: bool,
    /// For a newtype header's value (DIM-9): what the value is, and the
    /// number of trailing path steps inside the header.
    header: Option<(String, usize)>,
}

struct Cond {
    expr: Vec<String>,
    text: String,
    /// The variable a length binds that must equal `expr`; with none,
    /// `expr` must be non-negative.
    len: Option<usize>,
    /// A number the variable `len` names must be a multiple of, in place
    /// of `expr`.
    multiple: Option<i128>,
    /// A variable no slot binds, whose value is the one that makes `expr`
    /// equal `len`'s value; `expr` never decreases as it grows.
    root: Option<usize>,
}

/// Why a check failed: the input breaks a relation, or the program does
/// not match its own constraint.
#[derive(Debug)]
pub enum DimsError {
    Input(String),
    Internal(String),
}

impl std::fmt::Display for DimsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DimsError::Input(m) | DimsError::Internal(m) => f.write_str(m),
        }
    }
}

pub struct Dims {
    name: String,
    vars: Vec<String>,
    bind: Vec<Bind>,
    conds: Vec<Cond>,
}

impl Dims {
    /// Read the value of command `cmd`'s `dims` constraint.
    pub fn parse(v: &serde_json::Value, cmd: &str) -> Result<Dims, String> {
        let bad = || format!("malformed dims constraint in command '{}'", cmd);
        let strs = |k: &str| -> Result<Vec<String>, String> {
            v.get(k)
                .and_then(|x| x.as_array())
                .ok_or_else(bad)?
                .iter()
                .map(|x| x.as_str().map(str::to_string).ok_or_else(bad))
                .collect()
        };
        let vars = strs("vars")?;
        let mut bind = Vec::new();
        for b in v.get("bind").and_then(|x| x.as_array()).ok_or_else(bad)? {
            let num = |k: &str| b.get(k).and_then(|x| x.as_u64()).map(|x| x as usize).ok_or_else(bad);
            let mut path = Vec::new();
            for s in b.get("path").and_then(|x| x.as_array()).ok_or_else(bad)? {
                path.push(match s.as_str().ok_or_else(bad)? {
                    "*" => Step::Each,
                    "?" => Step::Some,
                    f => Step::Field(f.strip_prefix('f').and_then(|k| k.parse().ok()).ok_or_else(bad)?),
                });
            }
            let var = num("var")?;
            if var >= vars.len() {
                return Err(bad());
            }
            let header = match b.get("header") {
                None => None,
                Some(h) => {
                    let desc = h.get("desc").and_then(|x| x.as_str()).ok_or_else(bad)?;
                    let depth = h.get("depth").and_then(|x| x.as_u64()).ok_or_else(bad)? as usize;
                    if depth > path.len() {
                        return Err(bad());
                    }
                    Some((desc.to_string(), depth))
                }
            };
            bind.push(Bind {
                var,
                arg: num("arg")?,
                path,
                value: b.get("of").and_then(|x| x.as_str()) == Some("value"),
                header,
            });
        }
        let mut conds = Vec::new();
        for c in v.get("conds").and_then(|x| x.as_array()).ok_or_else(bad)? {
            let expr = c
                .get("expr")
                .and_then(|x| x.as_array())
                .ok_or_else(bad)?
                .iter()
                .map(|x| x.as_str().map(str::to_string).ok_or_else(bad))
                .collect::<Result<_, _>>()?;
            let text = c.get("text").and_then(|x| x.as_str()).ok_or_else(bad)?.to_string();
            let len = match c.get("len") {
                None => None,
                Some(x) => Some(x.as_u64().map(|k| k as usize).filter(|k| *k < vars.len()).ok_or_else(bad)?),
            };
            let multiple = match c.get("multiple") {
                None => None,
                Some(x) => Some(x.as_i64().filter(|m| *m > 0 && len.is_some()).ok_or_else(bad)? as i128),
            };
            let root = match c.get("root") {
                None => None,
                Some(x) => Some(x.as_u64().map(|k| k as usize).filter(|k| *k < vars.len() && len.is_some()).ok_or_else(bad)?),
            };
            conds.push(Cond { expr, text, len, multiple, root });
        }
        let name = v.get("name").and_then(|x| x.as_str()).unwrap_or(cmd).to_string();
        Ok(Dims { name, vars, bind, conds })
    }

    /// A variable as the signature writes it; one a length expression
    /// binds is named for the expression.
    fn var_name(&self, k: usize) -> &str {
        let v = &self.vars[k];
        v.strip_prefix('#').unwrap_or(v)
    }

    /// The arguments the relations read.
    pub fn args(&self) -> impl Iterator<Item = usize> + '_ {
        self.bind.iter().map(|b| b.arg)
    }

    /// Check the relations on the decoded arguments: `args[i]` is
    /// argument `i`'s value and schema, or `None` if no binding reads it.
    ///
    /// # Safety
    /// Each pointer heads a value laid out by its schema.
    pub unsafe fn check(
        &self,
        args: &[Option<(*const u8, &Schema)>],
        label: Label,
        resolve: Resolve,
    ) -> Result<(), DimsError> {
        let mut found: Vec<Option<(i128, String)>> = vec![None; self.vars.len()];
        for b in &self.bind {
            let Some(Some((ptr, schema))) = args.get(b.arg) else {
                return Err(DimsError::Internal(format!(
                    "internal error: argument #{} of '{}' was not decoded",
                    b.arg, self.name
                )));
            };
            let mut out = Vec::new();
            read(*ptr, schema, &b.path, b.value, String::new(), resolve, &mut out)?;
            let field = match (b.path.first(), schema.serial_type) {
                (Some(Step::Field(k)), SerialType::Map) => schema.keys.get(*k).map(String::as_str),
                _ => None,
            };
            for (v, at) in out {
                let (what, at) = match &b.header {
                    Some((desc, depth)) => {
                        let mut at = at.as_str();
                        for _ in 0..*depth {
                            at = at.rsplit_once('.').map_or(at, |(a, _)| a);
                        }
                        (format!("the value of {} of", desc), at.to_string())
                    }
                    None if b.value => ("the value of".to_string(), at),
                    None if b.path.is_empty() && schema.serial_type == SerialType::Table => {
                        ("the row count of".to_string(), at)
                    }
                    None => ("the length of".to_string(), at),
                };
                let (name, said) = label(b.arg, field);
                let loc = format!("{} {}{}", what, name, at.strip_prefix(said.as_str()).unwrap_or(&at));
                let var = self.var_name(b.var);
                match &found[b.var] {
                    None => found[b.var] = Some((v, loc)),
                    Some((w, first)) if *w != v => {
                        return Err(DimsError::Input(format!(
                            "dimension mismatch in '{}': {} is {} from {}, but {} is {}",
                            self.name, var, w, first, loc, v
                        )))
                    }
                    Some(_) => {}
                }
            }
        }
        for c in &self.conds {
            let length = c.len.and_then(|k| found[k].as_ref());
            if c.len.is_some() && length.is_none() {
                continue;
            }
            if let (Some(r), Some((l, loc))) = (c.root, length) {
                let (l, loc) = (*l, loc.clone());
                match root_of(&c.expr, r, l, &mut found) {
                    Root::Found(u) => found[r] = Some((u, format!("from {}", loc))),
                    Root::Unknown => {}
                    Root::None => {
                        return Err(DimsError::Input(format!(
                            "dimension mismatch in '{}': {} is {}, but its type requires {} for some {}, and no {} gives {}",
                            self.name,
                            loc,
                            l,
                            c.text,
                            self.var_name(r),
                            self.var_name(r),
                            l
                        )))
                    }
                }
                continue;
            }
            if let (Some(m), Some((l, loc))) = (c.multiple, length) {
                if l % m != 0 {
                    return Err(DimsError::Input(format!(
                        "dimension mismatch in '{}': {} is {}, but its type requires {}, a multiple of {}",
                        self.name, loc, l, c.text, m
                    )));
                }
                continue;
            }
            match eval(&c.expr, &found) {
                Eval::Value(x) if length.map_or(x >= 0, |(l, _)| *l == x) => {}
                Eval::Unbound => {}
                r => {
                    let mut used: Vec<usize> = c
                        .expr
                        .iter()
                        .filter_map(|t| t.strip_prefix('v').and_then(|k| k.parse().ok()))
                        .collect();
                    used.sort();
                    used.dedup();
                    let given: Vec<String> = used
                        .iter()
                        .filter_map(|&k| {
                            found.get(k)?.as_ref().map(|(v, loc)| format!("{} = {} ({})", self.var_name(k), v, loc))
                        })
                        .collect();
                    let why = match r {
                        Eval::DivZero => " (it divides by zero)",
                        Eval::Overflow => " (its value is out of range)",
                        _ => "",
                    };
                    let with = if given.is_empty() { String::new() } else { format!(", with {}", given.join(", ")) };
                    return Err(DimsError::Input(match (length, r) {
                        (Some((l, loc)), Eval::Value(_)) if used.is_empty() => format!(
                            "dimension mismatch in '{}': {} is {}, but its type requires {}",
                            self.name, loc, l, c.text
                        ),
                        (Some((l, loc)), Eval::Value(x)) => format!(
                            "dimension mismatch in '{}': {} is {}, but its type requires {} = {}{}",
                            self.name,
                            loc,
                            l,
                            c.text,
                            x,
                            with
                        ),
                        (Some((l, loc)), _) => format!(
                            "dimension mismatch in '{}': {} is {}, but its type requires {}{}{}",
                            self.name, loc, l, c.text, why, with
                        ),
                        _ => format!(
                            "dimension mismatch in '{}': {} >= 0 does not hold{}{}",
                            self.name, c.text, why, with
                        ),
                    }));
                }
            }
        }
        Ok(())
    }
}

enum Root {
    Found(i128),
    /// No natural gives the target.
    None,
    /// The search cannot run: another variable has no value, or the
    /// target is negative, which a condition of its own reports.
    Unknown,
}

/// The natural `u` for which `expr`, with variable `r` set to `u`, is
/// `target`, found by bisection over `0..=target`, which holds it when
/// `expr` never decreases in `u` and is at least `u`.
fn root_of(expr: &[String], r: usize, target: i128, found: &mut [Option<(i128, String)>]) -> Root {
    if target < 0 {
        return Root::Unknown;
    }
    let mut at = |u: i128, found: &mut [Option<(i128, String)>]| {
        found[r] = Some((u, String::new()));
        let v = eval(expr, found);
        found[r] = None;
        v
    };
    let (mut lo, mut hi) = (0i128, target);
    while lo <= hi {
        let mid = lo + (hi - lo) / 2;
        match at(mid, found) {
            Eval::Value(x) if x == target => return Root::Found(mid),
            Eval::Value(x) if x < target => lo = mid + 1,
            Eval::Value(_) | Eval::Overflow => hi = mid - 1,
            Eval::Unbound | Eval::DivZero => return Root::Unknown,
        }
    }
    Root::None
}

enum Eval {
    Value(i128),
    Unbound,
    DivZero,
    Overflow,
}

fn eval(expr: &[String], found: &[Option<(i128, String)>]) -> Eval {
    let mut stack: Vec<i128> = Vec::new();
    for t in expr {
        if let Some(k) = t.strip_prefix('v').and_then(|k| k.parse::<usize>().ok()) {
            match found.get(k) {
                Some(Some((v, _))) => stack.push(*v),
                _ => return Eval::Unbound,
            }
        } else if let Ok(n) = t.parse::<i128>() {
            stack.push(n);
        } else {
            let (Some(b), Some(a)) = (stack.pop(), stack.pop()) else {
                return Eval::Overflow;
            };
            let r = match t.as_str() {
                "+" => a.checked_add(b),
                "-" => a.checked_sub(b),
                "*" => a.checked_mul(b),
                // Division truncates toward zero, as KIND-3 says.
                "/" if b == 0 => return Eval::DivZero,
                "/" => a.checked_div(b),
                _ => None,
            };
            match r {
                Some(x) => stack.push(x),
                None => return Eval::Overflow,
            }
        }
    }
    match stack.as_slice() {
        [x] => Eval::Value(*x),
        _ => Eval::Overflow,
    }
}

/// The lengths or integer values at `path` from the value at `ptr`, each
/// with the indices that reach it.
unsafe fn read(
    ptr: *const u8,
    s: &Schema,
    path: &[Step],
    value: bool,
    at: String,
    resolve: Resolve,
    out: &mut Vec<(i128, String)>,
) -> Result<(), DimsError> {
    let mismatch = || DimsError::Internal("internal error: a dims path does not match its argument's schema".into());
    match path.split_first() {
        None if value => out.push((int_value(ptr, s, resolve)?, at)),
        None if s.serial_type == SerialType::Table => {
            if std::ptr::read_unaligned(ptr as *const u32) != TABLE_MAGIC {
                return Err(DimsError::Internal("internal error: a table argument is not an Arrow block".into()));
            }
            out.push((std::ptr::read_unaligned(ptr.add(TABLE_ROWS_AT) as *const u64) as i128, at));
        }
        None => {
            if s.serial_type != SerialType::Array {
                return Err(mismatch());
            }
            let a = std::ptr::read_unaligned(ptr as *const Array);
            out.push((a.size as i128, at));
        }
        Some((Step::Each, rest)) => {
            if s.serial_type != SerialType::Array || s.parameters.is_empty() {
                return Err(mismatch());
            }
            let a = std::ptr::read_unaligned(ptr as *const Array);
            if a.size == 0 {
                return Ok(());
            }
            let base = resolve(a.data).map_err(DimsError::Internal)?;
            let e = &s.parameters[0];
            for i in 0..a.size {
                read(base.add(i * e.width), e, rest, value, format!("{}[{}]", at, i), resolve, out)?;
            }
        }
        Some((Step::Field(k), rest)) => {
            if !matches!(s.serial_type, SerialType::Tuple | SerialType::Map) {
                return Err(mismatch());
            }
            let (Some(off), Some(f)) = (s.offsets.get(*k), s.parameters.get(*k)) else {
                return Err(mismatch());
            };
            let field = match s.keys.get(*k) {
                Some(key) => format!("{}.{}", at, key),
                None => format!("{}.{}", at, k),
            };
            read(ptr.add(*off), f, rest, value, field, resolve, out)?;
        }
        Some((Step::Some, rest)) => {
            if s.serial_type != SerialType::Optional || s.parameters.is_empty() {
                return Err(mismatch());
            }
            let rel = std::ptr::read_unaligned(ptr as *const RelPtr);
            if rel != RELNULL {
                read(resolve(rel).map_err(DimsError::Internal)?, &s.parameters[0], rest, value, at, resolve, out)?;
            }
        }
    }
    Ok(())
}

unsafe fn int_value(ptr: *const u8, s: &Schema, resolve: Resolve) -> Result<i128, DimsError> {
    let r = |n: usize| -> i128 {
        match n {
            1 => std::ptr::read_unaligned(ptr as *const i8) as i128,
            2 => std::ptr::read_unaligned(ptr as *const i16) as i128,
            4 => std::ptr::read_unaligned(ptr as *const i32) as i128,
            _ => std::ptr::read_unaligned(ptr as *const i64) as i128,
        }
    };
    Ok(match s.serial_type {
        SerialType::Sint8 => r(1),
        SerialType::Sint16 => r(2),
        SerialType::Sint32 => r(4),
        SerialType::Sint64 => r(8),
        SerialType::Uint8 => std::ptr::read_unaligned(ptr) as i128,
        SerialType::Uint16 => std::ptr::read_unaligned(ptr as *const u16) as i128,
        SerialType::Uint32 => std::ptr::read_unaligned(ptr as *const u32) as i128,
        SerialType::Uint64 => std::ptr::read_unaligned(ptr as *const u64) as i128,
        SerialType::Int => {
            // Inline layout: [size, value] for one limb, [size, relptr] for more.
            let size = std::ptr::read_unaligned(ptr as *const usize);
            match size {
                0 => 0,
                1 => std::ptr::read_unaligned(ptr.add(8) as *const i64) as i128,
                2 => {
                    let limbs = resolve(std::ptr::read_unaligned(ptr.add(8) as *const RelPtr)).map_err(DimsError::Internal)? as *const u64;
                    let lo = std::ptr::read_unaligned(limbs) as u128;
                    let hi = std::ptr::read_unaligned(limbs.add(1)) as u128;
                    ((hi << 64) | lo) as i128
                }
                _ => return Err(DimsError::Input("a label's value is too large to check its dimensions".into())),
            }
        }
        SerialType::Nil
        | SerialType::Bool
        | SerialType::Float32
        | SerialType::Float64
        | SerialType::String
        | SerialType::Array
        | SerialType::Tuple
        | SerialType::Map
        | SerialType::Optional
        | SerialType::Table
        | SerialType::Recur
        | SerialType::IFile
        | SerialType::OStream
        | SerialType::IStream
        | SerialType::Variant
        | SerialType::Enum => {
            return Err(DimsError::Internal("internal error: a label in a dims constraint is not an integer".into()))
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::schema::parse_schema;

    fn dims(v: &str) -> Dims {
        Dims::parse(&serde_json::from_str(v).unwrap(), "f").unwrap()
    }

    fn label(i: usize, _: Option<&str>) -> (String, String) {
        (format!("#{}", i), String::new())
    }

    // An array header whose data pointer is the address itself, so that
    // `resolve` is the identity.
    fn array(elems: &[i64]) -> (Array, Vec<i64>) {
        let data = elems.to_vec();
        (Array { size: data.len(), data: data.as_ptr() as RelPtr }, data)
    }

    fn run(d: &Dims, args: &[(*const u8, &Schema)]) -> Result<(), String> {
        let vals: Vec<Option<(*const u8, &Schema)>> = args.iter().map(|a| Some(*a)).collect();
        let resolve = |r: RelPtr| Ok(r as *const u8);
        unsafe { d.check(&vals, &label, &resolve) }.map_err(|e| e.to_string())
    }

    #[test]
    fn a_shared_length_must_agree() {
        let d = dims(r#"{"vars":["n"],"bind":[{"var":0,"arg":0,"path":[],"of":"length"},{"var":0,"arg":1,"path":[],"of":"length"}],"conds":[]}"#);
        let s = parse_schema("ai8").unwrap();
        let (a, _ka) = array(&[1, 2]);
        let (b, _kb) = array(&[3, 4]);
        let (c, _kc) = array(&[5]);
        let p = |x: &Array| x as *const Array as *const u8;
        assert!(run(&d, &[(p(&a), &s), (p(&b), &s)]).is_ok());
        let e = run(&d, &[(p(&a), &s), (p(&c), &s)]).unwrap_err();
        assert!(e.contains("n is 2 from the length of #0, but the length of #1 is 1"), "{e}");
    }

    #[test]
    fn a_condition_divides_toward_zero() {
        // (w - 7) / 2 >= 0 holds at w = 6, since -1 / 2 is 0 toward zero.
        let d = dims(r#"{"vars":["w"],"bind":[{"var":0,"arg":0,"path":[],"of":"value"}],"conds":[{"expr":["v0","7","-","2","/"],"text":"t"}]}"#);
        let s = parse_schema("i8").unwrap();
        let w: i64 = 6;
        assert!(run(&d, &[(&w as *const i64 as *const u8, &s)]).is_ok());
        let w: i64 = 4;
        assert!(run(&d, &[(&w as *const i64 as *const u8, &s)]).unwrap_err().contains("t >= 0 does not hold"));
    }

    #[test]
    fn a_zero_divisor_and_overflow_fail() {
        let found = vec![Some((0i128, String::new()))];
        assert!(matches!(eval(&["1".into(), "v0".into(), "/".into()], &found), Eval::DivZero));
        let big = vec![Some((i128::MAX, String::new()))];
        assert!(matches!(eval(&["v0".into(), "1".into(), "+".into()], &big), Eval::Overflow));
        assert!(matches!(eval(&["v1".into()], &found), Eval::Unbound));
    }

    #[test]
    fn a_length_expression_must_equal_its_value() {
        // f :: Vector n Int -> Vector (n + 1) Int
        let d = dims(r##"{"vars":["n","#n + 1"],"bind":[{"var":0,"arg":0,"path":[],"of":"length"},{"var":1,"arg":1,"path":[],"of":"length"}],"conds":[{"len":1,"expr":["v0","1","+"],"text":"n + 1"}]}"##);
        let s = parse_schema("ai8").unwrap();
        let (a, _ka) = array(&[1, 2]);
        let (b, _kb) = array(&[1, 2, 3]);
        let (c, _kc) = array(&[1]);
        let p = |x: &Array| x as *const Array as *const u8;
        assert!(run(&d, &[(p(&a), &s), (p(&b), &s)]).is_ok());
        let e = run(&d, &[(p(&a), &s), (p(&c), &s)]).unwrap_err();
        assert!(e.contains("the length of #1 is 1, but its type requires n + 1 = 3, with n = 2"), "{e}");
    }

    #[test]
    fn every_element_of_a_list_is_read() {
        let d = dims(r#"{"vars":["n"],"bind":[{"var":0,"arg":0,"path":["*"],"of":"length"}],"conds":[]}"#);
        let s = parse_schema("aai8").unwrap();
        let (x, _kx) = array(&[1, 2]);
        let (y, _ky) = array(&[3]);
        let rows = [x, y];
        let outer = Array { size: 2, data: rows.as_ptr() as RelPtr };
        let e = run(&d, &[(&outer as *const Array as *const u8, &s)]).unwrap_err();
        assert!(e.contains("the length of #0[1] is 1"), "{e}");
        let empty = Array { size: 0, data: RELNULL };
        assert!(run(&d, &[(&empty as *const Array as *const u8, &s)]).is_ok());
    }

    #[test]
    fn a_header_value_binds_its_dimension() {
        // f :: M m k Real -> M k n Real -> Int, with M's header ((m@Int, n@Int), ...)
        #[repr(C)]
        struct Hdr {
            m: i64,
            n: i64,
            xs: Array,
        }
        let d = dims(r##"{"vars":["m","k","#m * k"],"bind":[
            {"var":0,"arg":0,"path":["f0","f0"],"of":"value","header":{"desc":"m in the M header","depth":2}},
            {"var":1,"arg":0,"path":["f0","f1"],"of":"value","header":{"desc":"n in the M header","depth":2}},
            {"var":2,"arg":0,"path":["f1"],"of":"length"},
            {"var":1,"arg":1,"path":["f0","f0"],"of":"value","header":{"desc":"m in the M header","depth":2}}],
            "conds":[{"expr":["v0"],"text":"m"},{"len":2,"expr":["v0","v1","*"],"text":"m * k"}]}"##);
        let s = parse_schema("t2t2i8i8ai8").unwrap();
        let (xs, _k) = array(&[1, 2, 3, 4, 5, 6]);
        let a = Hdr { m: 2, n: 3, xs };
        let (ys, _k2) = array(&[1, 2, 3]);
        let b = Hdr { m: 3, n: 1, xs: ys };
        let c = Hdr { m: 2, n: 1, xs: Array { size: 2, data: b.xs.data } };
        let p = |x: &Hdr| x as *const Hdr as *const u8;
        assert!(run(&d, &[(p(&a), &s), (p(&b), &s)]).is_ok());
        let e = run(&d, &[(p(&a), &s), (p(&c), &s)]).unwrap_err();
        assert!(e.contains("k is 3 from the value of n in the M header of #0, but the value of m in the M header of #1 is 2"), "{e}");
        let short = Hdr { m: 2, n: 3, xs: Array { size: 5, data: a.xs.data } };
        let e = run(&d, &[(p(&short), &s), (p(&b), &s)]).unwrap_err();
        assert!(e.contains("the length of #0.1 is 5, but its type requires m * k = 6"), "{e}");
        let neg = Hdr { m: -1, n: 3, xs: Array { size: 0, data: RELNULL } };
        let e = run(&d, &[(p(&neg), &s), (p(&b), &s)]).unwrap_err();
        assert!(e.contains("m >= 0 does not hold, with m = -1"), "{e}");
    }

    #[test]
    fn a_literal_dimension_names_only_itself() {
        let d = dims(r##"{"vars":["#2"],"bind":[{"var":0,"arg":0,"path":[],"of":"value"}],"conds":[{"len":0,"expr":["2"],"text":"2"}]}"##);
        let s = parse_schema("i8").unwrap();
        let w: i64 = 3;
        let e = run(&d, &[(&w as *const i64 as *const u8, &s)]).unwrap_err();
        assert!(e.ends_with("the value of #0 is 3, but its type requires 2"), "{e}");
    }

    #[test]
    fn a_table_binds_its_row_count() {
        let d = dims(r#"{"vars":["n"],"bind":[{"var":0,"arg":0,"path":[],"of":"length"},{"var":0,"arg":1,"path":[],"of":"length"}],"conds":[]}"#);
        let t = parse_schema("T").unwrap();
        let s = parse_schema("ai8").unwrap();
        let mut block = [0u8; 64];
        block[..4].copy_from_slice(&TABLE_MAGIC.to_ne_bytes());
        block[TABLE_ROWS_AT..TABLE_ROWS_AT + 8].copy_from_slice(&2u64.to_ne_bytes());
        let (a, _ka) = array(&[1, 2]);
        let (b, _kb) = array(&[1]);
        let p = |x: &Array| x as *const Array as *const u8;
        assert!(run(&d, &[(block.as_ptr(), &t), (p(&a), &s)]).is_ok());
        let e = run(&d, &[(block.as_ptr(), &t), (p(&b), &s)]).unwrap_err();
        assert!(e.contains("n is 2 from the row count of #0, but the length of #1 is 1"), "{e}");
    }

    #[test]
    fn a_length_must_be_a_multiple() {
        // f :: Vector (2 * n) Int -> Int
        let d = dims(r##"{"vars":["#2 * n"],"bind":[{"var":0,"arg":0,"path":[],"of":"length"}],"conds":[{"len":0,"multiple":2,"expr":[],"text":"2 * n"}]}"##);
        let s = parse_schema("ai8").unwrap();
        let (a, _ka) = array(&[1, 2]);
        let (b, _kb) = array(&[1, 2, 3]);
        let p = |x: &Array| x as *const Array as *const u8;
        assert!(run(&d, &[(p(&a), &s)]).is_ok());
        let e = run(&d, &[(p(&b), &s)]).unwrap_err();
        assert!(e.ends_with("the length of #0 is 3, but its type requires 2 * n, a multiple of 2"), "{e}");
    }

    #[test]
    fn a_length_finds_the_variable_it_needs() {
        // f :: Vector (n * n) Int -> Int
        let d = dims(r##"{"vars":["#n * n","n"],"bind":[{"var":0,"arg":0,"path":[],"of":"length"}],"conds":[{"len":0,"root":1,"expr":["v1","v1","*"],"text":"n * n"}]}"##);
        let s = parse_schema("ai8").unwrap();
        let (a, _ka) = array(&[1, 2, 3, 4]);
        let (b, _kb) = array(&[1, 2, 3]);
        let (c, _kc) = array(&[]);
        let p = |x: &Array| x as *const Array as *const u8;
        assert!(run(&d, &[(p(&a), &s)]).is_ok());
        assert!(run(&d, &[(p(&c), &s)]).is_ok());
        let e = run(&d, &[(p(&b), &s)]).unwrap_err();
        assert!(e.ends_with("the length of #0 is 3, but its type requires n * n for some n, and no n gives 3"), "{e}");
    }

    #[test]
    fn a_search_waits_for_its_other_variables() {
        // f :: Vector (n * n + m) Int -> [Vector m Int] -> Int, the list empty
        let d = dims(r##"{"vars":["#n * n + m","m","n"],"bind":[{"var":0,"arg":0,"path":[],"of":"length"},{"var":1,"arg":1,"path":["*"],"of":"length"}],"conds":[{"len":0,"root":2,"expr":["v2","v2","*","v1","+"],"text":"n * n + m"}]}"##);
        let s = parse_schema("ai8").unwrap();
        let ls = parse_schema("aai8").unwrap();
        let (a, _ka) = array(&[1, 2, 3]);
        let empty = Array { size: 0, data: RELNULL };
        let p = |x: &Array| x as *const Array as *const u8;
        assert!(run(&d, &[(p(&a), &s), (p(&empty), &ls)]).is_ok());
    }
}
