#[derive(Clone, Debug)]
pub enum MyHolder<T> {
    Hollow,
    Hold(std::boxed::Box<(T,)>),
}

pub fn mk(n: u64) -> Vec<u8> { vec![7u8; n as usize] }
pub fn len(xs: &Vec<u8>) -> u64 { xs.len() as u64 }
pub fn add(a: u64, b: u64) -> u64 { a + b }
pub fn fail(n: u64) -> Vec<u8> { panic!("no batch {}", n) }
