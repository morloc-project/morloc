// A native type that is not Clone; only its packer gives it a wire form.
pub struct Tally {
    v: i64,
}

pub fn pack_tally(x: i64) -> Tally {
    Tally { v: x }
}

pub fn unpack_tally(t: &Tally) -> i64 {
    t.v
}

pub fn mk_list(n: i64) -> Vec<Tally> {
    (1..=n).map(|v| Tally { v }).collect()
}

pub fn produce<F: Fn(&Vec<Tally>)>(sink: F) {
    sink(&vec![Tally { v: 1 }, Tally { v: 2 }]);
}

pub fn show_all(xs: &Vec<Tally>) -> String {
    let parts: Vec<String> = xs.iter().map(|t| t.v.to_string()).collect();
    format!("rust {}", parts.join(" "))
}
