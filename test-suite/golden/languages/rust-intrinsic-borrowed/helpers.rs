pub fn cat_str(a: &String, b: &String) -> String {
    format!("{}{}", a, b)
}

pub fn rs_map<A, B, F: Fn(&A) -> B>(f: F, xs: &[A]) -> Vec<B> {
    xs.iter().map(f).collect()
}
