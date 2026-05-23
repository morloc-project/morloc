//! Recursive-record env stack shared by every walker that descends a
//! Schema tree. Each walker pushes a `&<name>` declaration onto the
//! stack on entry and pops on exit; back-references (MORLOC_RECUR
//! / `SerialType::Recur`) resolve their target by linear scan from
//! the top of the stack.
//!
//! The stack stores raw `*const Schema` pointers rather than borrowed
//! references because all walkers thread the same `&mut Vec<...>`
//! while taking shared borrows of various sub-schemas during the
//! traversal. The pointers are always derived from live `&Schema`
//! values that outlive the walk (they come from the top-level Schema
//! held on the caller's stack), so dereferencing them is safe as long
//! as the walk doesn't mutate the Schema -- which it doesn't.

use crate::error::MorlocError;
use crate::schema::{Schema, SerialType};

/// Stack of in-scope named-schema declarations.
pub type RecurEnv = Vec<(String, *const Schema)>;

/// Look up the most recent declaration of `name` on the env stack.
///
/// Returns a clear error rather than `Option` because every caller
/// needs the error path -- a dangling back-reference is a wire-format
/// or codegen bug, not a recoverable absence.
pub fn lookup(env: &RecurEnv, name: &str) -> Result<*const Schema, MorlocError> {
    env.iter()
        .rev()
        .find(|(n, _)| n == name)
        .map(|(_, s)| *s)
        .ok_or_else(|| {
            MorlocError::Schema(format!(
                "Recur back-reference to undeclared name '{name}'"
            ))
        })
}

/// Run `body` with `schema` pushed onto the env stack for the
/// duration of the call. Recur nodes are not pushed (they carry the
/// `name` field as a lookup key, not a binding site).
///
/// The closure body receives `&mut RecurEnv` so it can pass the env
/// to recursive walker calls. The push/pop happens around the
/// closure invocation; an early-return via `?` inside the closure
/// still triggers the pop (Result is captured before pop runs).
pub fn with_scope<F, R>(env: &mut RecurEnv, schema: &Schema, body: F) -> R
where
    F: FnOnce(&mut RecurEnv) -> R,
{
    let pushed = match (schema.serial_type, schema.name.as_deref()) {
        (SerialType::Recur, _) => false,
        (_, Some(n)) => {
            env.push((n.to_string(), schema as *const Schema));
            true
        }
        _ => false,
    };
    let result = body(env);
    if pushed {
        env.pop();
    }
    result
}
