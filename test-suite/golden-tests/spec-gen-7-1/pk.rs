pub fn packVec(xs: &Vec<f64>) -> ndarray::Array1<f64> { ndarray::Array1::from(xs.clone()) }
pub fn unpackVec(a: &ndarray::Array1<f64>) -> Vec<f64> { a.to_vec() }
