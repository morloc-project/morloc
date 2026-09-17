//! Deep recursive values for tests, built without recursion, so each walker
//! over a value can be run at a depth no thread stack could walk one frame
//! per level.
//!
//! Every builder writes a chain of `depth` nodes directly into the shared
//! memory arena, bottom up, and returns the root and the parsed schema. The
//! layouts are the wire layouts the walkers read: a record is its fields at
//! `schema.offsets`, an optional is one relative pointer, a variant is a tag
//! byte and a relative pointer to the arm's tuple, an array is a header
//! pointing at its inline elements.

use crate::schema::{parse_schema, Schema};
use crate::shm::{self, AbsPtr, Array, RelPtr, RELNULL};

/// A record chain `LL { head: Int64, tail: ?LL }`, `head` counting up from
/// the innermost node.
pub const LL_SCHEMA: &str = "&2LLm24headi84tail?^2LL";
/// A variant chain `Tree = Leaf | Node Int64 Tree Tree` along the right
/// branch, with a `Leaf` on every left branch.
pub const TREE_SCHEMA: &str = "&4Treev24Leaf04Node3i8^4Tree^4Tree";
/// Two variants alternating: `A = Nil | ACons Int64 B`, `B = BCons Int64 A`.
pub const MUTUAL_SCHEMA: &str = "&1Av23Nil05ACons2i8&1Bv15BCons2i8^1A";
/// A record whose one child sits inline in an array: `Rose { v: Int64,
/// kids: [Rose] }`, each node holding exactly one kid.
pub const ROSE_SCHEMA: &str = "&4Rosem21vi84kidsa^4Rose";

/// The depth every deep test uses. A 1 MiB arena grows as needed.
pub const DEPTH: usize = 100_000;

/// Run `f` on a thread far smaller than any pool worker's, so a walk that
/// spends a frame per level fails here before it could anywhere else.
pub fn on_small_stack<F: FnOnce() + Send + 'static>(f: F) {
    std::thread::Builder::new()
        .stack_size(256 * 1024)
        .spawn(f)
        .unwrap()
        .join()
        .unwrap();
}

unsafe fn put<T: Copy>(p: AbsPtr, off: usize, v: T) {
    std::ptr::write_unaligned(p.add(off) as *mut T, v);
}

/// Build the `LL` chain. Returns the root node.
pub fn build_ll(depth: usize) -> (AbsPtr, Schema) {
    let schema = parse_schema(LL_SCHEMA).unwrap();
    let w = schema.width;
    let mut prev: RelPtr = RELNULL;
    let mut node = std::ptr::null_mut();
    for i in 0..depth {
        node = shm::shcalloc(1, w).unwrap();
        unsafe {
            put(node, schema.offsets[0], i as i64);
            put(node, schema.offsets[1], prev);
        }
        prev = shm::abs2rel(node).unwrap();
    }
    (node, schema)
}

/// Build the `Tree` chain. Returns the root variant slot (16 bytes).
pub fn build_tree(depth: usize) -> (AbsPtr, Schema) {
    let schema = parse_schema(TREE_SCHEMA).unwrap();
    let node_arm = &schema.parameters[1];
    let slot = shm::shcalloc(1, schema.width).unwrap();
    unsafe {
        put(slot, 0, 0u8);
        put(slot, 8, RELNULL);
    }
    let mut cur = slot;
    for i in 0..depth {
        let payload = shm::shcalloc(1, node_arm.width).unwrap();
        unsafe {
            put(payload, node_arm.offsets[0], i as i64);
            put(payload, node_arm.offsets[1], 0u8);
            put(payload, node_arm.offsets[1] + 8, RELNULL);
            std::ptr::copy_nonoverlapping(cur, payload.add(node_arm.offsets[2]), schema.width);
        }
        let next = shm::shcalloc(1, schema.width).unwrap();
        unsafe {
            put(next, 0, 1u8);
            put(next, 8, shm::abs2rel(payload).unwrap());
        }
        cur = next;
    }
    (cur, schema)
}

/// Build the alternating `A`/`B` chain. Returns the root `A` slot.
pub fn build_mutual(depth: usize) -> (AbsPtr, Schema) {
    let schema = parse_schema(MUTUAL_SCHEMA).unwrap();
    let acons = &schema.parameters[1];
    let b = &acons.parameters[1];
    let bcons = &b.parameters[0];
    let mut a = shm::shcalloc(1, schema.width).unwrap();
    unsafe {
        put(a, 0, 0u8);
        put(a, 8, RELNULL);
    }
    for i in 0..depth {
        // B holding the current A, then a new A holding that B.
        let bp = shm::shcalloc(1, bcons.width).unwrap();
        let bslot = shm::shcalloc(1, b.width).unwrap();
        let ap = shm::shcalloc(1, acons.width).unwrap();
        let aslot = shm::shcalloc(1, schema.width).unwrap();
        unsafe {
            put(bp, bcons.offsets[0], i as i64);
            std::ptr::copy_nonoverlapping(a, bp.add(bcons.offsets[1]), schema.width);
            put(bslot, 0, 0u8);
            put(bslot, 8, shm::abs2rel(bp).unwrap());
            put(ap, acons.offsets[0], i as i64);
            std::ptr::copy_nonoverlapping(bslot, ap.add(acons.offsets[1]), b.width);
            put(aslot, 0, 1u8);
            put(aslot, 8, shm::abs2rel(ap).unwrap());
        }
        a = aslot;
    }
    (a, schema)
}

/// Build the caterpillar `Rose`. Returns the root record.
pub fn build_rose(depth: usize) -> (AbsPtr, Schema) {
    let schema = parse_schema(ROSE_SCHEMA).unwrap();
    let kids = &schema.parameters[1];
    let w = schema.width;
    // The innermost node has no kids.
    let mut node = shm::shcalloc(1, w).unwrap();
    unsafe {
        put(node, schema.offsets[0], 0i64);
        put(node, schema.offsets[1], Array { size: 0, data: RELNULL });
    }
    for i in 1..=depth {
        // One element, laid out inline in the array's data block.
        let elems = shm::shcalloc(1, w).unwrap();
        unsafe { std::ptr::copy_nonoverlapping(node, elems, w) };
        let parent = shm::shcalloc(1, w).unwrap();
        unsafe {
            put(parent, schema.offsets[0], i as i64);
            put(parent, schema.offsets[1], Array { size: 1, data: shm::abs2rel(elems).unwrap() });
        }
        node = parent;
    }
    let _ = kids;
    (node, schema)
}

/// Count the `LL` chain by following `tail` in a loop.
pub fn count_ll(root: AbsPtr, schema: &Schema) -> usize {
    let mut n = 0;
    let mut cur = root;
    loop {
        n += 1;
        let tail: RelPtr = unsafe { std::ptr::read_unaligned(cur.add(schema.offsets[1]) as *const RelPtr) };
        if tail == RELNULL {
            return n;
        }
        cur = shm::rel2abs(tail).unwrap();
    }
}

use crate::schema::SerialType;

/// A small deterministic generator of JSON values for a schema.
pub struct Gen(pub u64);
impl Gen {
    pub fn next(&mut self) -> u64 {
        self.0 ^= self.0 << 13;
        self.0 ^= self.0 >> 7;
        self.0 ^= self.0 << 17;
        self.0
    }
    pub fn below(&mut self, n: u64) -> u64 {
        self.next() % n
    }
    pub fn value(&mut self, s: &Schema, root: &Schema, depth: usize) -> String {
        match s.serial_type {
            SerialType::Nil => "null".into(),
            SerialType::Bool => if self.below(2) == 0 { "true" } else { "false" }.into(),
            SerialType::Sint8 | SerialType::Sint16 | SerialType::Sint32 | SerialType::Sint64 =>
                format!("{}", self.below(200) as i64 - 100),
            SerialType::Uint8 | SerialType::Uint16 | SerialType::Uint32 | SerialType::Uint64 =>
                format!("{}", self.below(200)),
            SerialType::Float32 | SerialType::Float64 => format!("{}.5", self.below(50) as i64 - 25),
            SerialType::Int => match self.below(3) {
                0 => format!("{}", self.below(1000) as i64 - 500),
                1 => "123456789012345678901234567890".into(),
                _ => "-98765432109876543210".into(),
            },
            SerialType::String => {
                let words = ["", "a", "hello world", r"tab\tnew\nline", r#"quote\"q"#, r"\u00e9t\u00e9", "[{,}]"];
                format!("\"{}\"", words[self.below(words.len() as u64) as usize])
            }
            SerialType::Enum => format!("\"{}\"", s.keys[self.below(s.keys.len() as u64) as usize]),
            SerialType::Array => {
                let n = if depth > 6 { 0 } else { self.below(4) };
                let items: Vec<String> = (0..n).map(|_| self.value(&s.parameters[0], root, depth + 1)).collect();
                format!("[{}]", items.join(", "))
            }
            SerialType::Tuple => {
                let items: Vec<String> = s.parameters.iter().map(|p| self.value(p, root, depth + 1)).collect();
                format!("[{}]", items.join(","))
            }
            SerialType::Map => {
                if self.below(4) == 0 {
                    let items: Vec<String> = s.parameters.iter().map(|p| self.value(p, root, depth + 1)).collect();
                    format!("[{}]", items.join(","))
                } else {
                    // Members in a shuffled order.
                    let mut idx: Vec<usize> = (0..s.parameters.len()).collect();
                    for i in (1..idx.len()).rev() {
                        let j = self.below(i as u64 + 1) as usize;
                        idx.swap(i, j);
                    }
                    let items: Vec<String> = idx
                        .iter()
                        .map(|&i| format!("\"{}\" : {}", s.keys[i], self.value(&s.parameters[i], root, depth + 1)))
                        .collect();
                    format!("{{ {} }}", items.join(" , "))
                }
            }
            SerialType::Optional => {
                if depth > 6 || self.below(3) == 0 {
                    "null".into()
                } else {
                    self.value(&s.parameters[0], root, depth + 1)
                }
            }
            SerialType::Variant => {
                let mut arms: Vec<usize> = if depth > 6 {
                    (0..s.keys.len()).filter(|&i| s.parameters[i].size == 0).collect()
                } else {
                    (0..s.keys.len()).collect()
                };
                // A type whose every arm carries fields ends through
                // the type it holds.
                if arms.is_empty() {
                    arms = (0..s.keys.len()).collect();
                }
                let i = arms[self.below(arms.len() as u64) as usize];
                if s.parameters[i].size == 0 {
                    format!("\"{}\"", s.keys[i])
                } else {
                    format!("{{\"{}\":{}}}", s.keys[i], self.value(&s.parameters[i], root, depth + 1))
                }
            }
            SerialType::Recur => {
                let target = crate::recur::Resolver::new(root);
                let t = target.resolve(s).unwrap();
                // The declaration is a node of the root tree; walk it.
                self.value(t, root, depth + 1)
            }
            _ => "null".into(),
        }
    }
}


/// The schema shapes the random tests cover: every leaf and container
/// kind, records in both wire forms, and every recursive shape.
pub const SHAPES: &[&str] = &[
    "i4", "s", "as", "t3si4s", "aai4", "t2?i4s", "m22idj4tagsas", "v23Nil04Cons2i4s", "e21A1B",
    "&2LLm24headi84tail?^2LL", "&4Treev24Leaf04Node3i8^4Tree^4Tree",
    "&1Av23Nil05ACons2i8&1Bv15BCons2i8^1A", "&4Rosem21vi84kidsa^4Rose", "a&2LLm24headi84tail?^2LL",
    "m31af81b?s1cat2i4?i4", "?v23Nil04Cons2i4s",
];

#[cfg(test)]
mod tests {
    use super::*;

    /// Count an `LL` chain laid out in a self-contained buffer whose relptrs
    /// are offsets from the buffer's start.
    fn count_ll_in_buffer(buf: &[u8], root: usize, schema: &Schema) -> usize {
        let mut n = 0;
        let mut cur = root;
        loop {
            n += 1;
            let off = cur + schema.offsets[1];
            let tail = RelPtr::from_ne_bytes(buf[off..off + 8].try_into().unwrap());
            if tail == RELNULL {
                return n;
            }
            cur = tail as usize;
        }
    }

    #[test]
    fn flatten_rebase_and_shift_are_flat_over_deep_chains() {
        on_small_stack(|| {
            let _shm = crate::init_test_shm();
            for (root, schema, count) in [
                {
                    let (r, s) = build_ll(DEPTH);
                    (r, s, DEPTH)
                },
            ] {
                let flat = crate::voidstar::flatten_to_buffer(root, &schema).unwrap();
                assert_eq!(count_ll_in_buffer(&flat, 0, &schema), count);
                // Into fresh SHM: every pointer rebased from the buffer.
                let back = crate::voidstar::read_binary(&flat, &schema).unwrap();
                assert_eq!(count_ll(back, &schema), count);
                // Moved 64 bytes down inside a buffer: every pointer shifted.
                let mut moved = vec![0u8; 64];
                moved.extend_from_slice(&flat);
                unsafe { crate::voidstar::shift_buffer_relptrs(moved.as_mut_ptr(), 64, &schema, 64).unwrap() };
                assert_eq!(count_ll_in_buffer(&moved, 64, &schema), count);
            }
            // The other shapes flatten and come back with the same size.
            for (root, schema) in [build_tree(DEPTH), build_mutual(DEPTH), build_rose(DEPTH)] {
                let size = crate::ffi::calc_voidstar_size_inner(root, &schema).unwrap();
                let flat = crate::voidstar::flatten_to_buffer(root, &schema).unwrap();
                assert_eq!(flat.len(), size);
                let back = crate::voidstar::read_binary(&flat, &schema).unwrap();
                assert_eq!(crate::ffi::calc_voidstar_size_inner(back, &schema).unwrap(), size);
                let again = crate::voidstar::flatten_to_buffer(back, &schema).unwrap();
                assert_eq!(again, flat);
            }
        });
    }

    #[test]
    fn json_writer_is_flat_over_deep_chains() {
        on_small_stack(|| {
            let _shm = crate::init_test_shm();
            let (ll, lls) = build_ll(DEPTH);
            let text = crate::json::voidstar_to_json_string(ll, &lls).unwrap();
            // {"head":N,"tail":  per level, then null and the closing braces.
            assert!(text.starts_with(&format!("{{\"head\":{},\"tail\":{{\"head\":{},", DEPTH - 1, DEPTH - 2)));
            assert!(text.ends_with(&format!("{{\"head\":0,\"tail\":null{}", "}".repeat(DEPTH))));
            let (tree, ts) = build_tree(DEPTH);
            let text = crate::json::voidstar_to_json_string(tree, &ts).unwrap();
            assert!(text.starts_with(&format!("{{\"Node\":[{},\"Leaf\",{{\"Node\":[", DEPTH - 1)));
            assert!(text.ends_with(&format!("\"Leaf\"]}}{}", "]}".repeat(DEPTH - 1))));
            let (a, ms) = build_mutual(DEPTH);
            let text = crate::json::voidstar_to_json_string(a, &ms).unwrap();
            assert!(text.ends_with(&format!("\"Nil\"]}}{}", "]}".repeat(2 * DEPTH - 1))));
            let (rose, rs) = build_rose(DEPTH);
            let text = crate::json::voidstar_to_json_string(rose, &rs).unwrap();
            assert!(text.ends_with(&format!("{{\"v\":0,\"kids\":[]}}{}", "]}".repeat(DEPTH))));
            // Pretty output indents every level, so its size grows with the
            // square of the depth; a shallower chain keeps the test quick.
            let (ll, lls) = build_ll(2000);
            let pretty = crate::json::pretty_json_string(ll, &lls).unwrap();
            assert!(pretty.ends_with(&format!("\"tail\": null\n{}", (0..2000).rev().map(|d| format!("{}}}\n", " ".repeat(2 * d))).collect::<String>().trim_end_matches('\n'))));
        });
    }

    /// The JSON text of an `LL` chain of `depth` nodes.
    fn ll_json(depth: usize) -> String {
        let mut t = String::with_capacity(depth * 24);
        for i in (0..depth).rev() {
            t.push_str(&format!("{{\"head\":{i},\"tail\":"));
        }
        t.push_str("null");
        t.push_str(&"}".repeat(depth));
        t
    }

    #[test]
    fn json_loader_is_flat_and_linear_over_deep_chains() {
        on_small_stack(|| {
            let _shm = crate::init_test_shm();
            let schema = parse_schema(LL_SCHEMA).unwrap();
            let root = crate::json::read_json_with_schema(&ll_json(DEPTH), &schema).unwrap();
            assert_eq!(count_ll(root, &schema), DEPTH);
            // A variant chain, a mutual chain and a caterpillar written by
            // the JSON writer load back to the same bytes.
            for (root, schema) in [build_tree(DEPTH), build_mutual(DEPTH), build_rose(DEPTH)] {
                let text = crate::json::voidstar_to_json_string(root, &schema).unwrap();
                let back = crate::json::read_json_with_schema(&text, &schema).unwrap();
                assert_eq!(
                    crate::voidstar::flatten_to_buffer(back, &schema).unwrap(),
                    crate::voidstar::flatten_to_buffer(root, &schema).unwrap()
                );
            }
            // Four times the depth must not cost far more than four times
            // the time: the load scans its text once.
            let time = |d: usize| {
                let text = ll_json(d);
                let t = std::time::Instant::now();
                for _ in 0..3 {
                    crate::json::read_json_with_schema(&text, &schema).unwrap();
                }
                t.elapsed()
            };
            let small = time(20_000);
            let large = time(80_000);
            assert!(large < small * 8, "load time grew from {small:?} to {large:?} for 4x the depth");
        });
    }

    #[test]
    fn flat_writer_is_flat_and_linear_over_deep_chains() {
        on_small_stack(|| {
            let _shm = crate::init_test_shm();
            for (root, schema) in [build_ll(DEPTH), build_tree(DEPTH), build_mutual(DEPTH), build_rose(DEPTH)] {
                let flat = crate::voidstar::flatten_to_buffer(root, &schema).unwrap();
                let mut w0: Vec<u8> = Vec::new();
                let n = crate::voidstar::write_flat_to_writer_with_vol_idx(&mut w0, root, &schema, 0).unwrap();
                assert_eq!(n, w0.len());
                assert!(n <= flat.len());
                assert_eq!(&flat[..n], &w0[..]);
                assert!(flat[n..].iter().all(|&b| b == 0));
                // With a volume index baked in, the stream reads back to
                // the same value.
                let mut w7: Vec<u8> = Vec::new();
                crate::voidstar::write_flat_to_writer_with_vol_idx(&mut w7, root, &schema, 7).unwrap();
                assert_eq!(w7.len(), n);
                let back = crate::voidstar::read_binary_with_hint(&w7, &schema, 7).unwrap();
                assert_eq!(crate::voidstar::flatten_to_buffer(back, &schema).unwrap(), flat);
            }
            // Four times the depth must not cost far more than four times
            // the time.
            let time = |d: usize| {
                let (root, schema) = build_ll(d);
                let t = std::time::Instant::now();
                for _ in 0..3 {
                    let mut w: Vec<u8> = Vec::new();
                    crate::voidstar::write_flat_to_writer(&mut w, root, &schema).unwrap();
                }
                t.elapsed()
            };
            let small = time(20_000);
            let large = time(80_000);
            assert!(large < small * 8, "write time grew from {small:?} to {large:?} for 4x the depth");
        });
    }

    /// Random values of every shape stream to the same bytes the in-memory
    /// flatten produces and read back, at either volume index, to the same
    /// value.
    #[test]
    fn flat_writer_matches_flatten_on_random_values() {
        let _shm = crate::init_test_shm();
        let mut g = Gen(0x2545f4914f6cdd1d);
        for schema_str in SHAPES {
            let schema = parse_schema(schema_str).unwrap();
            for _ in 0..40 {
                let text = g.value(&schema, &schema, 0);
                let ptr = crate::json::read_json_with_schema(&text, &schema).unwrap();
                let flat = crate::voidstar::flatten_to_buffer(ptr, &schema).unwrap();
                let printed = crate::json::voidstar_to_json_string(ptr, &schema).unwrap();
                for vol in [0u16, 7] {
                    let mut w: Vec<u8> = Vec::new();
                    let n = crate::voidstar::write_flat_to_writer_with_vol_idx(&mut w, ptr, &schema, vol).unwrap();
                    assert_eq!(n, w.len());
                    if vol == 0 {
                        assert_eq!(&flat[..n], &w[..], "{schema_str}: {text}");
                    }
                    let back = crate::voidstar::read_binary_with_hint(&w, &schema, vol).unwrap();
                    assert_eq!(crate::json::voidstar_to_json_string(back, &schema).unwrap(), printed, "{schema_str}: {text} at vol {vol}");
                }
            }
        }
    }

    #[test]
    fn msgpack_hash_and_deep_copy_are_flat_over_deep_chains() {
        on_small_stack(|| {
            let _shm = crate::init_test_shm();
            for (root, schema) in [build_ll(DEPTH), build_tree(DEPTH), build_mutual(DEPTH), build_rose(DEPTH)] {
                let flat = crate::voidstar::flatten_to_buffer(root, &schema).unwrap();
                // Through msgpack and back to the same bytes.
                let packed = crate::mpack::pack_with_schema(root, &schema).unwrap();
                let back = crate::mpack::unpack_with_schema(&packed, &schema).unwrap();
                assert_eq!(crate::voidstar::flatten_to_buffer(back, &schema).unwrap(), flat);
                // The hash is a function of the value, not of where it lies.
                let h = crate::cache::hash_voidstar_value(root, &schema, 0).unwrap();
                assert_eq!(crate::cache::hash_voidstar_value(back, &schema, 0).unwrap(), h);
                assert_ne!(crate::cache::hash_voidstar_value(root, &schema, 1).unwrap(), h);
                // A deep copy into a fresh slot reproduces the value.
                let dst = shm::shmalloc(schema.width).unwrap();
                unsafe { crate::voidstar::deep_copy(root, dst, &schema).unwrap() };
                assert_eq!(crate::voidstar::flatten_to_buffer(dst, &schema).unwrap(), flat);
            }
            // The hash of a chain one link shorter differs.
            let (a, s) = build_ll(DEPTH);
            let (b, _) = build_ll(DEPTH - 1);
            assert_ne!(
                crate::cache::hash_voidstar_value(a, &s, 0).unwrap(),
                crate::cache::hash_voidstar_value(b, &s, 0).unwrap()
            );
        });
    }

    #[test]
    fn size_walk_is_flat_over_deep_chains() {
        on_small_stack(|| {
            let _shm = crate::init_test_shm();
            let (ll, lls) = build_ll(DEPTH);
            // Root record (16), then per link the pointer's alignment slack
            // beyond the slot (7) and the next record (16).
            let expect = 16 + 23 * (DEPTH - 1);
            assert_eq!(crate::ffi::calc_voidstar_size_inner(ll, &lls).unwrap(), expect);
            let (tree, ts) = build_tree(DEPTH);
            // Leaf slot (16), then per node the slot with its padding (23)
            // and the arm tuple (40) less the child slot it holds (16).
            let expect = 16 + 47 * DEPTH;
            assert_eq!(crate::ffi::calc_voidstar_size_inner(tree, &ts).unwrap(), expect);
            let (a, ms) = build_mutual(DEPTH);
            assert!(crate::ffi::calc_voidstar_size_inner(a, &ms).unwrap() > 16 * DEPTH);
            let (rose, rs) = build_rose(DEPTH);
            // Root record (24), then per level the one-element region
            // (7 of alignment slack plus 24) holding the child record.
            let expect = 24 + (7 + 24) * DEPTH;
            assert_eq!(crate::ffi::calc_voidstar_size_inner(rose, &rs).unwrap(), expect);
            // Bounded mode stops early and reports a size past the bound.
            let bounded = crate::ffi::calc_voidstar_size_bounded(ll, &lls, 1024).unwrap();
            assert!(bounded > 1024 && bounded < expect);
        });
    }

    #[test]
    fn builders_produce_chains_of_the_requested_depth() {
        let _shm = crate::init_test_shm();
        let (root, schema) = build_ll(1000);
        assert_eq!(count_ll(root, &schema), 1000);
        let (tree, ts) = build_tree(50);
        let arm = &ts.parameters[1];
        let mut n = 0;
        let mut slot = tree;
        loop {
            let tag = unsafe { *slot };
            if tag == 0 {
                break;
            }
            n += 1;
            let rel: RelPtr = unsafe { std::ptr::read_unaligned(slot.add(8) as *const RelPtr) };
            slot = unsafe { shm::rel2abs(rel).unwrap().add(arm.offsets[2]) };
        }
        assert_eq!(n, 50);
        let (_a, ms) = build_mutual(10);
        assert_eq!(ms.name.as_deref(), Some("A"));
        let (rose, rs) = build_rose(10);
        let hdr: Array = unsafe { std::ptr::read_unaligned(rose.add(rs.offsets[1]) as *const Array) };
        assert_eq!(hdr.size, 1);
    }
}
