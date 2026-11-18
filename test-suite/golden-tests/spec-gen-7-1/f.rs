use crate::mlc::types::main::Sample;
pub fn total(s: &Sample) -> f64 { s.v.sum() * s.n as f64 }
