pub fn helper_panics(x: i64) -> i64 {
    let v: Vec<i64> = vec![10, 20];
    v[x as usize]
}

pub fn add_one(x: i64) -> i64 {
    x + 1
}

pub fn text_panics(s: &str, x: i64) -> i64 {
    let v: Vec<i64> = vec![10, 20];
    v[x as usize] + s.len() as i64
}

pub fn curried_panics(x: i64) -> impl Fn(&i64) -> i64 {
    move |y: &i64| {
        let v: Vec<i64> = vec![10, 20];
        v[(x + *y) as usize]
    }
}
