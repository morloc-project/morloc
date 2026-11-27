use ndarray::{Array1, Array2};

pub fn dot(a: &[f64], b: &[f64]) -> f64 {
    Array1::from_vec(a.to_vec()).dot(&Array1::from_vec(b.to_vec()))
}

pub fn matmul(a: &((i64, i64), Vec<f64>), b: &((i64, i64), Vec<f64>)) -> ((i64, i64), Vec<f64>) {
    let ((ar, ac), ad) = a;
    let ((br, bc), bd) = b;
    let am = Array2::from_shape_vec((*ar as usize, *ac as usize), ad.clone())
        .unwrap_or_else(|_| rustmorloc::morloc_throw("matmul: bad left shape"));
    let bm = Array2::from_shape_vec((*br as usize, *bc as usize), bd.clone())
        .unwrap_or_else(|_| rustmorloc::morloc_throw("matmul: bad right shape"));
    let cm = am.dot(&bm);
    ((*ar, *bc), cm.iter().cloned().collect())
}
