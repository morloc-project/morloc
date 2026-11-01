use std::sync::Arc;

pub fn apply2<K: Fn(&i64, &i64) -> i64>(k: K, a: i64, b: i64) -> i64 {
    k(&a, &b)
}

pub fn feed<K: Fn(&Arc<dyn Fn(&i64, &i64) -> i64 + Send + Sync>) -> i64>(k: K) -> i64 {
    let f: Arc<dyn Fn(&i64, &i64) -> i64 + Send + Sync> = Arc::new(|a: &i64, b: &i64| a - b);
    k(&f)
}

pub fn make_sub(n: i64) -> impl Fn(&i64, &i64) -> i64 + Send + Sync + 'static {
    move |a: &i64, b: &i64| (a - b) * n
}

pub fn first2<C>(fs: &Vec<Arc<dyn Fn(&i64, &i64) -> C + Send + Sync>>, x: i64) -> C {
    fs[0](&x, &x)
}

pub fn feed_split<C, K>(k: K) -> C
where
    K: Fn(&Arc<dyn Fn(&i64) -> Arc<dyn Fn(&i64) -> i64 + Send + Sync> + Send + Sync>) -> C,
{
    let f: Arc<dyn Fn(&i64) -> Arc<dyn Fn(&i64) -> i64 + Send + Sync> + Send + Sync> =
        Arc::new(|a: &i64| {
            let a = *a;
            Arc::new(move |y: &i64| a * 10 + y) as Arc<dyn Fn(&i64) -> i64 + Send + Sync>
        });
    k(&f)
}
