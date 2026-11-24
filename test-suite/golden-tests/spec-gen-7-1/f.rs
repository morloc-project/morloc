use crate::mlc::types::_local::main::Sample;
pub fn total(s: &Sample) -> f64 { s.v.sum() * s.n as f64 }
