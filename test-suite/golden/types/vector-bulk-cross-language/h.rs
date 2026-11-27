pub fn mk_u8(n: u64) -> Vec<u8> { (0..n).map(|i| (i * 7 % 251) as u8).collect() }
pub fn mk_i32(n: u64) -> Vec<i32> { (0..n).map(|i| (i as i32) - 500_000).collect() }
pub fn mk_f32(n: u64) -> Vec<f32> { (0..n).map(|i| (i % 1000) as f32 * 0.5).collect() }
pub fn mk_real(n: u64) -> Vec<f64> { (0..n).map(|i| i as f64 * 0.25).collect() }
pub fn mk_bool(n: u64) -> Vec<bool> { (0..n).map(|i| i % 3 == 0).collect() }

pub fn sum_u8(xs: &Vec<u8>) -> u64 { xs.iter().enumerate().map(|(i, &x)| (i as u64 % 13 + 1) * x as u64).sum() }
pub fn sum_i32(xs: &Vec<i32>) -> i64 { xs.iter().enumerate().map(|(i, &x)| (i as i64 % 13 + 1) * x as i64).sum() }
pub fn sum_f32(xs: &Vec<f32>) -> f64 { xs.iter().enumerate().map(|(i, &x)| (i % 13 + 1) as f64 * x as f64).sum() }
pub fn sum_real(xs: &Vec<f64>) -> f64 { xs.iter().enumerate().map(|(i, &x)| (i % 13 + 1) as f64 * x).sum() }
pub fn count_bool(xs: &Vec<bool>) -> u64 { xs.iter().enumerate().map(|(i, &x)| if x { i as u64 % 13 + 1 } else { 0 }).sum() }

pub fn snan(n: u64) -> f32 { f32::from_bits(0x7f80_0000 | (n as u32 & 0x3f_ffff)) }
pub fn snan_vec(n: u64) -> Vec<f32> { (1..=n).map(snan).collect() }
pub fn bits(x: f32) -> u32 { x.to_bits() }
pub fn bits_vec(xs: &Vec<f32>) -> Vec<u32> { xs.iter().map(|x| x.to_bits()).collect() }
