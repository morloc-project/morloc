use std::collections::BTreeSet;
pub fn bag_pack(xs: &Vec<i64>) -> BTreeSet<i64> { xs.iter().cloned().collect() }
pub fn bag_unpack(s: &BTreeSet<i64>) -> Vec<i64> { s.iter().cloned().collect() }
pub fn mk(i: i64) -> Vec<i64> { vec![i, i + 10] }
pub fn zero_acc() -> BTreeSet<i64> { BTreeSet::new() }
pub fn add_batch(a: &BTreeSet<i64>, b: &Vec<i64>) -> BTreeSet<i64> { let mut r = a.clone(); r.extend(b.iter().cloned()); r }
pub fn merge_acc(a: &BTreeSet<i64>, b: &BTreeSet<i64>) -> BTreeSet<i64> { a.union(b).cloned().collect() }
pub fn show_acc(a: &BTreeSet<i64>) -> String { a.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(" ") }
pub fn mk_bags(n: i64) -> Vec<BTreeSet<i64>> { (0..n).map(|i| [i, i + 1].into_iter().collect()).collect() }
pub fn count_bags(xs: &Vec<BTreeSet<i64>>) -> i64 { xs.len() as i64 }
pub fn add_int(a: i64, b: &Vec<i64>) -> i64 { a + b.iter().sum::<i64>() }
pub fn merge_int(a: i64, b: i64) -> i64 { a + b }
pub fn show_int(a: i64) -> String { a.to_string() }
pub fn zero_int() -> i64 { 0 }
