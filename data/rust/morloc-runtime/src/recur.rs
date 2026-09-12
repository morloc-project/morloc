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

/// An env holding only `schema`'s own declaration, for a walk that
/// starts INSIDE a named schema rather than at it -- a record's fields
/// taken one at a time, say -- so a back-reference to the schema itself
/// still resolves.
pub fn self_scope(schema: &Schema) -> RecurEnv {
    match (schema.serial_type, schema.name.as_deref()) {
        (SerialType::Recur, _) => Vec::new(),
        (_, Some(n)) => vec![(n.to_string(), schema as *const Schema)],
        _ => Vec::new(),
    }
}

/// Make a sub-schema self-contained by replacing every back-reference to
/// `parent`'s declaration with `parent` itself. A field schema handed to a
/// reader on its own has no enclosing declaration to resolve against; with
/// the parent spliced in, the parent's own back-references then sit under
/// the declaration they need. The parent's widths are already patched, so
/// the clone is complete as it stands.
pub fn reroot_under(parent: &Schema, child: &Schema) -> Schema {
    match (parent.serial_type, parent.name.as_deref()) {
        (SerialType::Recur, _) | (_, None) => child.clone(),
        (_, Some(name)) => splice(name, parent, child),
    }
}

fn splice(name: &str, parent: &Schema, s: &Schema) -> Schema {
    if s.serial_type == SerialType::Recur && s.name.as_deref() == Some(name) {
        return parent.clone();
    }
    let mut out = s.clone();
    out.parameters = s.parameters.iter().map(|c| splice(name, parent, c)).collect();
    out
}

/// True when a back-reference to `name` occurs anywhere in `s`.
pub fn refers_to(s: &Schema, name: &str) -> bool {
    (s.serial_type == SerialType::Recur && s.name.as_deref() == Some(name))
        || s.parameters.iter().any(|c| refers_to(c, name))
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::schema::{parse_schema, schema_to_string};

    // A linked chain: a record whose `next` field is an optional
    // back-reference to the record.
    const CHAIN: &str = "&4Nodem25labels4next?^4Node";

    #[test]
    fn a_field_that_refers_back_is_rerooted_under_its_record() {
        let node = parse_schema(CHAIN).unwrap();
        let next = &node.parameters[1];
        assert!(refers_to(next, "Node"));
        assert!(!refers_to(&node.parameters[0], "Node"));
        let rooted = reroot_under(&node, next);
        // The reference is now a declaration the field's own schema
        // contains, so the string parses on its own and round-trips.
        let s = schema_to_string(&rooted);
        assert_eq!(s, "?&4Nodem25labels4next?^4Node");
        let reparsed = parse_schema(&s).unwrap();
        assert_eq!(schema_to_string(&reparsed), s);
    }

    #[test]
    fn a_walk_started_inside_a_record_sees_the_record() {
        let node = parse_schema(CHAIN).unwrap();
        let env = self_scope(&node);
        assert!(lookup(&env, "Node").is_ok());
        assert!(lookup(&Vec::new(), "Node").is_err());
    }
}
