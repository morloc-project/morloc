#[derive(Clone, Debug)]
pub struct PInner { pub ann: String, pub x: i64 }
#[derive(Clone, Debug)]
pub struct POuter { pub ann: String, pub inner: Option<PInner> }
#[derive(Clone, Debug)]
pub struct Inner<C> { pub ann: C, pub x: i64 }
#[derive(Clone, Debug)]
pub struct Outer<T, C> { pub ann: T, pub inner: Option<Inner<C>> }

fn count_inner<T, C>(xs: &Vec<Outer<T, C>>) -> i64 { xs.iter().filter(|o| o.inner.is_some()).count() as i64 }
fn count_pinner(xs: &Vec<POuter>) -> i64 { xs.iter().filter(|o| o.inner.is_some()).count() as i64 }

#[derive(Clone, Debug)]
pub struct PNested { pub ann: String, pub inner: Option<Option<PInner>> }
fn count_nested(xs: &Vec<PNested>) -> i64 { xs.iter().filter(|o| o.inner.is_some()).count() as i64 }
fn nest_state(x: Option<Option<i64>>) -> String { if x.is_some() { "present".to_string() } else { "absent".to_string() } }
