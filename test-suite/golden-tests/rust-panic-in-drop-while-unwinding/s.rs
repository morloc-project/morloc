struct PanicsOnDrop;
impl Drop for PanicsOnDrop {
    fn drop(&mut self) {
        panic!("second, in drop");
    }
}
pub fn twice(x: i64) -> i64 {
    let _d = PanicsOnDrop;
    if x > 0 { panic!("first"); }
    x
}
