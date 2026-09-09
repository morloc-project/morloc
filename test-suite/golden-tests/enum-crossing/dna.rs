// A user-mapped `data Rust => DNA = "DNA"` needs its definition here.
#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum DNA {
    A = 0,
    C = 1,
    G = 2,
    T = 3,
}

pub fn complement(x: DNA) -> DNA {
    match x {
        DNA::A => DNA::T,
        DNA::C => DNA::G,
        DNA::G => DNA::C,
        DNA::T => DNA::A,
    }
}
