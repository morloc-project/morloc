pub struct Loc { pub seqid: i64, pub start: i64 }
pub fn mka(x: i64) -> Loc { Loc { seqid: x, start: 10 * x } }
pub fn mkb(x: i64) -> Loc { Loc { seqid: x, start: 100 * x } }
pub fn starts(a: &Loc, b: &Loc) -> i64 { a.start + b.start }
