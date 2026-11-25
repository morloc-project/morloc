#[derive(Clone)]
pub struct Hit {
    pub name: String,
    pub score: u64,
}

pub fn produce_rows<F: Fn(&Vec<Vec<u64>>)>(b: i64, sink: F) {
    for i in 0..b {
        let batch: Vec<Vec<u64>> = (0..i).map(|j| vec![(i * 10 + j) as u64, j as u64]).collect();
        sink(&batch);
    }
}

pub fn produce_strs<F: Fn(&Vec<String>)>(b: i64, sink: F) {
    for i in 0..b {
        let mut batch: Vec<String> = (0..i).map(|j| format!("s{} {}", i, j)).collect();
        if i == 1 {
            batch.push(String::new());
        }
        sink(&batch);
    }
}

pub fn produce_hits<F: Fn(&Vec<Hit>)>(b: i64, sink: F) {
    for i in 0..b {
        let batch: Vec<Hit> = (0..i)
            .map(|j| Hit { name: format!("h{}{}", i, j), score: (i * 10 + j) as u64 })
            .collect();
        sink(&batch);
    }
}

pub fn show_rows(xs: &Vec<Vec<u64>>) -> String {
    let body: String = xs
        .iter()
        .map(|r| format!(" {}", r.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(",")))
        .collect();
    format!("rows {}:{}\n", xs.len(), body)
}

pub fn show_strs(xs: &Vec<String>) -> String {
    let body: String = xs.iter().map(|s| format!(" [{}]", s)).collect();
    format!("strs {}:{}\n", xs.len(), body)
}

pub fn show_hits(xs: &Vec<Hit>) -> String {
    let body: String = xs.iter().map(|h| format!(" {}={}", h.name, h.score)).collect();
    format!("hits {}:{}\n", xs.len(), body)
}
