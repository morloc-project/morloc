//! The nexus side of the command-entry dimension checks (DIM-3): finds a
//! command's `dims` constraint and names its arguments as the command
//! line does.

use morloc_runtime_types::dims::Dims;
use morloc_runtime_types::schema::Schema;
use morloc_runtime_types::shm_types::RelPtr;

use crate::manifest::{Arg, Command};

extern "C" {
    fn rel2abs(ptr: RelPtr, errmsg: *mut *mut std::ffi::c_char) -> *mut std::ffi::c_void;
}

/// The command's `dims` constraint, if it has one.
pub fn of(cmd: &Command) -> Result<Option<Dims>, String> {
    match cmd.constraints.iter().find(|c| c.ctype == "dims") {
        None => Ok(None),
        Some(c) => match &c.value {
            Some(v) => Dims::parse(v, &cmd.name).map(Some),
            None => Err(format!("malformed dims constraint in command '{}'", cmd.name)),
        },
    }
}

/// Check `d` on the decoded arguments of `cmd`.
///
/// # Safety
/// Each pointer heads a value laid out by its schema.
pub unsafe fn check(d: &Dims, cmd: &Command, args: &[Option<(*const u8, &Schema)>]) -> Result<(), String> {
    let resolve = |rel: RelPtr| -> Result<*const u8, String> {
        let mut err: *mut std::ffi::c_char = std::ptr::null_mut();
        let p = rel2abs(rel, &mut err);
        if p.is_null() {
            return Err(crate::process::take_c_errmsg(err).unwrap_or_else(|| "a pointer does not resolve".into()));
        }
        Ok(p as *const u8)
    };
    let label = |i: usize, field: Option<&str>| arg_label(cmd, i, field);
    d.check(args, &label, &resolve).map_err(|e| e.to_string())
}

/// The argument as a user names it, its option or its position, and the
/// leading part of a location the name already says.
fn arg_label(cmd: &Command, i: usize, field: Option<&str>) -> (String, String) {
    let at = format!("argument #{}", i);
    match cmd.args.get(i) {
        Some(Arg::Group { entries, .. }) => {
            let flag = field.and_then(|key| {
                let e = entries.iter().find(|e| e.key == key)?;
                Some((e.arg.long_opt_str()?, key))
            });
            match flag {
                Some((l, key)) => (format!("--{} ({})", l, at), format!(".{}", key)),
                None => (at, String::new()),
            }
        }
        Some(a) => match a.long_opt_str() {
            Some(l) => (format!("--{} ({})", l, at), String::new()),
            None => (at, String::new()),
        },
        None => (at, String::new()),
    }
}
