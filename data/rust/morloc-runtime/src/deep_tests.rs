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
