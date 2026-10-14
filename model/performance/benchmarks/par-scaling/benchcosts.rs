// Cost functions for the parallel benchmarks (see parallel/bench/main.loc).

pub fn mlcparbench_spin(n: i64) -> i64 {
    let mut x: u32 = 1;
    for _ in 0..n { x = x.wrapping_mul(1103515245).wrapping_add(12345) & 0x7FFF_FFFF; }
    (x % 1000) as i64
}

pub fn mlcparbench_spin_keep(n: i64) -> bool { mlcparbench_spin(n) % 2 == 0 }

pub fn mlcparbench_spin_list(n: i64) -> Vec<i64> {
    let v = mlcparbench_spin(n);
    vec![v; (v % 4) as usize]
}

pub fn mlcparbench_make_str(n: i64) -> String { "x".repeat(n as usize) }

pub fn mlcparbench_str_cost(s: &String) -> i64 { (s.len() % 1000) as i64 }
