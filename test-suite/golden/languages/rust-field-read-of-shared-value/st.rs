#[derive(Clone, Debug, PartialEq)]
pub struct St {
    pub maze: Vec<String>,
    pub level: i64,
}

pub fn st_new(n: i64) -> St {
    St { maze: vec!["ab".to_string(); n as usize], level: 1 }
}

pub fn st_maze(n: i64) -> Vec<String> {
    vec!["ab".to_string(); n as usize]
}

pub fn st_equal<A: PartialEq>(x: &A, y: &A, r: (i64, i64)) -> (i64, i64) {
    if x == y { (r.0, r.1 + 1) } else { (r.0 + 1, r.1) }
}
