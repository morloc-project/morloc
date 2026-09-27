use std::io::Write;

pub fn build(log: &String, n: i64) -> Vec<i64> {
    let mut f = std::fs::OpenOptions::new().append(true).create(true).open(log).unwrap();
    f.write_all(b"ran\n").unwrap();
    println!("building {}", n);
    std::io::stdout().flush().unwrap();
    (0..n).map(|i| i * i).collect()
}

pub fn as_lines(xs: &Vec<i64>) -> String {
    xs.iter().map(|x| format!("{}\n", x)).collect()
}

pub fn pad(width: i64, xs: &Vec<i64>) -> String {
    xs.iter().map(|x| format!("{:>w$}\n", x, w = width as usize)).collect()
}

pub fn sink_lines(xs: &Vec<i64>) {
    for x in xs {
        println!("sink {}", x);
    }
    std::io::stdout().flush().unwrap();
}

pub fn summary(xs: &Vec<i64>) -> i64 {
    xs.len() as i64
}
