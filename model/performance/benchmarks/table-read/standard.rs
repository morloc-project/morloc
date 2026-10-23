// Reference reader for the table-read benchmark: a plain Rust program that
// reads the benchmark's CSV into rows or columns and prints the row count.
use std::io::BufRead;

#[allow(dead_code)]
struct Gene {
    name: String,
    chrom: String,
    start: i64,
    end: i64,
    score: f64,
    keep: bool,
}

#[derive(Default)]
struct Columns {
    name: Vec<u8>,
    name_off: Vec<i32>,
    chrom: Vec<u8>,
    chrom_off: Vec<i32>,
    start: Vec<i64>,
    end: Vec<i64>,
    score: Vec<f64>,
    keep: Vec<bool>,
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let (mode, path, headed) = (&args[1], &args[2], args[3] == "headed");
    let f = std::fs::File::open(path).expect("open");
    let mut r = std::io::BufReader::with_capacity(1 << 16, f);
    let mut line = String::new();
    if headed {
        r.read_line(&mut line).expect("read");
    }
    let mut rows: Vec<Gene> = Vec::new();
    let mut cols = Columns::default();
    cols.name_off.push(0);
    cols.chrom_off.push(0);
    loop {
        line.clear();
        if r.read_line(&mut line).expect("read") == 0 {
            break;
        }
        let mut it = line.trim_end_matches('\n').split(',');
        let name = it.next().unwrap();
        let chrom = it.next().unwrap();
        let start: i64 = it.next().unwrap().parse().unwrap();
        let end: i64 = it.next().unwrap().parse().unwrap();
        let score: f64 = it.next().unwrap().parse().unwrap();
        let keep = match it.next().unwrap() {
            "true" => true,
            "false" => false,
            c => panic!("bad bool {}", c),
        };
        if mode == "records" {
            rows.push(Gene { name: name.to_string(), chrom: chrom.to_string(), start, end, score, keep });
        } else {
            cols.name.extend_from_slice(name.as_bytes());
            cols.name_off.push(cols.name.len() as i32);
            cols.chrom.extend_from_slice(chrom.as_bytes());
            cols.chrom_off.push(cols.chrom.len() as i32);
            cols.start.push(start);
            cols.end.push(end);
            cols.score.push(score);
            cols.keep.push(keep);
        }
    }
    let n = if mode == "records" { rows.len() } else { cols.start.len() };
    println!("{}", n);
}
