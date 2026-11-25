#[derive(Clone)]
pub struct P {
    pub a: i64,
    pub b: String,
}

pub fn p_id(x: &P) -> P {
    x.clone()
}
