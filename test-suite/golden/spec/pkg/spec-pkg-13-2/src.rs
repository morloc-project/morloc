#[link(name = "fake")]
extern "C" {
    fn fake_answer() -> i32;
}

pub fn answer(x: i64) -> i64 {
    x + unsafe { fake_answer() } as i64
}
