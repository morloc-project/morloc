use std::rc::Rc;
use std::sync::Arc;

pub fn make_counter(k: i64) -> Arc<dyn Fn(&i64) -> i64 + Send + Sync> {
    let base = Rc::new(k);
    Arc::new(move |x| x + *base)
}
