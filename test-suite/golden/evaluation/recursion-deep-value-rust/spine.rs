// Count the Node spine of a Tree by reference, without cloning or recursion.
pub fn spine_count(t: &Tree) -> i64 {
    let mut n = 0;
    let mut cur = t;
    while let Tree::Node(b) = cur {
        n += 1;
        cur = &b.2;
    }
    n
}
