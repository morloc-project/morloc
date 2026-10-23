pub fn use_list(fs: &Vec<std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>>, x: i64) -> i64 { fs[0](&x) + fs[1](&x) }
pub fn use_tuple(t: &(std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>, std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>), x: i64) -> i64 { (t.0)(&x) + (t.1)(&x) }
pub fn use_opt(f: &Option<std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>>, x: i64) -> i64 { match f { Some(g) => g(&x), None => x } }
