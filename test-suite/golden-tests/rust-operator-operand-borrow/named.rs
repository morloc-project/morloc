#[derive(Clone)]
pub struct Named {
    pub name: String,
    pub size: i64,
}

pub fn mkNamed(s: &String) -> Named {
    Named { name: s.clone(), size: 1 }
}
