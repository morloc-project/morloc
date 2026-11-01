#[derive(Clone, Debug)]
pub struct P {
    pub px: i64,
    pub py: f64,
}

// Both arguments have one static type, which is their native form.
pub fn same<T>(name: &String, _a: &T, _b: &T) -> String {
    format!("{} ok", name)
}

pub fn echo<T: Clone>(x: &T) -> T {
    x.clone()
}
