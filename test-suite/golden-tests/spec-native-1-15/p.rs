#[derive(Clone)]
pub struct PairT<A> { pub fst: A, pub snd: A }
#[derive(Clone)]
pub struct PointT { pub fst: f64, pub snd: f64 }
#[derive(Clone)]
pub struct WT<A> { pub fst: (A, A), pub snd: (A, A) }
pub fn packTenths(x: i64) -> f64 { x as f64 / 10.0 }
pub fn unpackTenths(x: f64) -> i64 { (x * 10.0 + 0.5) as i64 }
pub fn readPt(p: &PointT) -> i64 { (p.fst * 100.0 + 0.5) as i64 + (p.snd * 10.0 + 0.5) as i64 }
pub fn mkPt(n: i64) -> PointT { PointT { fst: n as f64 / 10.0, snd: (n + 1) as f64 / 10.0 } }
pub fn readPts(ps: &Vec<PointT>) -> i64 { ps.iter().map(readPt).sum() }
pub fn readOpt(p: &Option<PointT>) -> i64 { match p { Some(q) => readPt(q), None => -1 } }
pub fn readW(w: &WT<f64>) -> i64 { (w.fst.0 * 1000.0 + 0.5) as i64 + (w.snd.1 * 10.0 + 0.5) as i64 }
#[derive(Clone)]
pub struct WideT { pub a: f64, pub b: i64, pub c: i64, pub d: i64, pub e: i64, pub f: i64, pub g: i64, pub h: i64, pub i: i64 }
pub fn readWide(w: &WideT) -> i64 { w.b }
