use crate::mlc::types::_local::main::{Code, Pt};
pub fn code_len(c: &Code) -> i64 {
    c.sym + c.name.len() as i64 + c.pt.x + c.pt.y + c.items.iter().sum::<i64>()
}
pub fn mk_code(n: i64) -> Code {
    Code { sym: n, name: "ab".to_string(), items: vec![n, n], pt: Pt { x: 1, y: 2 } }
}
