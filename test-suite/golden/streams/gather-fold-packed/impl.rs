// No serializer knows this type; only its packer gives it a wire form.
pub struct Tally {
    v: i64,
}

pub fn pack_tally(x: i64) -> Tally {
    Tally { v: x }
}

pub fn unpack_tally(t: &Tally) -> i64 {
    t.v
}

extern "C" {
    fn fork() -> i32;
    fn waitpid(pid: i32, status: *mut i32, options: i32) -> i32;
    fn _exit(code: i32) -> !;
}

pub fn produce<F: Fn(&Vec<Tally>)>(sink: F) {
    sink(&vec![Tally { v: 1 }, Tally { v: 2 }]);
    sink(&vec![Tally { v: 3 }]);
}

pub fn produce_forks<F: Fn(&Vec<Tally>)>(sink: F) {
    sink(&vec![Tally { v: 1 }]);
    let pid = unsafe { fork() };
    if pid == 0 {
        let ok = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| sink(&vec![Tally { v: 10 }]))).is_ok();
        unsafe { _exit(if ok { 0 } else { 3 }) };
    }
    let mut status = 0;
    unsafe { waitpid(pid, &mut status, 0) };
    sink(&vec![Tally { v: 100 }]);
}

pub fn add_tallies(acc: i64, xs: &Vec<Tally>) -> i64 {
    acc + xs.iter().map(|t| t.v).sum::<i64>()
}

pub fn merge(a: i64, b: i64) -> i64 {
    a + b
}

pub fn show_n(n: i64) -> String {
    format!("rust n={}\n", n)
}
