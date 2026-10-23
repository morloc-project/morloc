#[derive(Clone)]
pub struct Ops {
    pub inc: std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>,
    pub scale: std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>,
}

pub fn rs_use_ops(ops: &Ops, n: i64) -> i64 {
    (ops.inc)(&n) + (ops.scale)(&n)
}

pub fn rs_use_list(fs: &Vec<std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>>, n: i64) -> i64 {
    fs[0](&n) + fs[1](&n)
}
