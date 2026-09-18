//! Back-reference resolution for walkers that descend a Schema tree.
//! `Resolver` binds every `Recur` node to its declaration once per walk,
//! so a step resolves a back-reference by one binary search and never
//! allocates; `reroot_under` makes a sub-schema self-contained for a
//! reader that starts inside a declaration.

use crate::error::MorlocError;
use crate::schema::{Schema, SerialType};

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

/// Static resolution of the back-references in one schema tree.
///
/// A back-reference names the nearest enclosing declaration of its name,
/// which is a property of the tree, so it is computed once per walk and
/// looked up by node address instead of pushing declarations as the walk
/// descends. The resolver also knows which nodes have no back-reference
/// anywhere below them: a walker steps those by direct call, since the
/// recursion is then bounded by the schema's height, and frames only the
/// nodes under which a value can be arbitrarily deep.
///
/// Node addresses are the keys, so the resolver is bound to the tree it
/// was built from and must not outlive it.
pub struct Resolver<'r> {
    /// Each `Recur` node with its declaration; a reference with no
    /// enclosing declaration is kept with a null target and rejected only
    /// when a walk reaches it.
    targets: Vec<(*const Schema, *const Schema)>,
    /// Every node of the tree whose subtree holds no `Recur`, sorted by
    /// address.
    shallow: Vec<*const Schema>,
    /// Whether the tree holds any `Recur` at all.
    recursive: bool,
    _tree: std::marker::PhantomData<&'r Schema>,
}

impl<'r> Resolver<'r> {
    pub fn new(root: &'r Schema) -> Resolver<'r> {
        let mut r = Resolver {
            targets: Vec::new(),
            shallow: Vec::new(),
            recursive: false,
            _tree: std::marker::PhantomData,
        };
        let mut decls: Vec<&Schema> = Vec::new();
        r.index(root, &mut decls);
        r.shallow.sort_unstable();
        r.targets.sort_unstable_by_key(|(k, _)| *k);
        r
    }

    /// Record `s` and its subtree; returns whether the subtree holds a
    /// back-reference.
    fn index(&mut self, s: &'r Schema, decls: &mut Vec<&'r Schema>) -> bool {
        if s.serial_type == SerialType::Recur {
            let target = decls
                .iter()
                .rev()
                .find(|d| d.name == s.name)
                .map_or(std::ptr::null(), |d| *d as *const Schema);
            self.targets.push((s as *const Schema, target));
            self.recursive = true;
            return true;
        }
        let declares = s.name.is_some();
        if declares {
            decls.push(s);
        }
        let mut deep = false;
        for p in &s.parameters {
            deep |= self.index(p, decls);
        }
        if declares {
            decls.pop();
        }
        if !deep {
            self.shallow.push(s as *const Schema);
        }
        deep
    }

    /// The node a walk should treat `s` as: a `Recur` becomes its
    /// declaration, anything else is itself.
    #[inline]
    pub fn resolve(&self, s: &'r Schema) -> Result<&'r Schema, MorlocError> {
        if s.serial_type != SerialType::Recur {
            return Ok(s);
        }
        let key = s as *const Schema;
        match self.targets.binary_search_by_key(&key, |(k, _)| *k) {
            Ok(i) if !self.targets[i].1.is_null() => {
                // SAFETY: the target is a node of the tree this resolver was
                // built from, which outlives the resolver.
                Ok(unsafe { &*self.targets[i].1 })
            }
            _ => Err(MorlocError::Schema(format!(
                "back-reference to '{}' has no enclosing declaration in this walk",
                s.name.as_deref().unwrap_or("")
            ))),
        }
    }

    /// True when no back-reference occurs under `s`, so a walk may step it
    /// by direct call. A node the resolver has not indexed is reported as
    /// deep: framing it costs a heap frame, while stepping it by call could
    /// recurse without bound.
    #[inline]
    pub fn flat(&self, s: &Schema) -> bool {
        self.shallow.binary_search(&(s as *const Schema)).is_ok()
    }

    /// True when the tree holds no back-reference at all.
    #[inline]
    pub fn trivial(&self) -> bool {
        !self.recursive
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::schema::{parse_schema, schema_to_string};

    #[test]
    fn resolver_binds_a_back_reference_to_the_nearest_declaration() {
        let node = parse_schema(CHAIN).unwrap();
        let r = Resolver::new(&node);
        assert!(!r.trivial());
        let recur = &node.parameters[1].parameters[0];
        assert_eq!(recur.serial_type, SerialType::Recur);
        assert!(std::ptr::eq(r.resolve(recur).unwrap(), &node));
        assert!(std::ptr::eq(r.resolve(&node.parameters[0]).unwrap(), &node.parameters[0]));
        // The label field has no back-reference below it; the record and
        // its optional field do.
        assert!(r.flat(&node.parameters[0]));
        assert!(!r.flat(&node));
        assert!(!r.flat(&node.parameters[1]));
    }

    #[test]
    fn resolver_keeps_nested_declarations_apart() {
        // An inner declaration reusing the outer name owns the references
        // under it.
        let s = parse_schema("&2T7m21vi44next?&2T7m21vs4next?^2T7").unwrap();
        let r = Resolver::new(&s);
        let inner = &s.parameters[1].parameters[0];
        let inner_recur = &inner.parameters[1].parameters[0];
        assert!(std::ptr::eq(r.resolve(inner_recur).unwrap(), inner));
    }

    #[test]
    fn resolver_rejects_a_foreign_node_as_deep_and_an_unbound_reference_lazily() {
        let s = parse_schema(CHAIN).unwrap();
        let r = Resolver::new(&s);
        let other = parse_schema("i4").unwrap();
        assert!(!r.flat(&other));
        // A field sliced out of its record has no declaration for its
        // back-reference; the error comes when a walk reaches it.
        let field = s.parameters[1].clone();
        let r2 = Resolver::new(&field);
        let recur = &field.parameters[0];
        assert!(r2.resolve(recur).is_err());
        let flat = parse_schema("at2si4").unwrap();
        let r3 = Resolver::new(&flat);
        assert!(r3.trivial());
        assert!(r3.flat(&flat));
        assert!(r3.flat(&flat.parameters[0]));
    }

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
}
