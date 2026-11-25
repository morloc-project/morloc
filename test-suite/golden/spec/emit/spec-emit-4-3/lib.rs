pub fn step(k: i64, i: i64) -> bool {
    if i > k {
        panic!("an operand after the deciding one ran")
    }
    i != k
}

pub fn hit(k: i64, i: i64) -> bool {
    if i > k {
        panic!("an operand after the deciding one ran")
    }
    i == k
}
