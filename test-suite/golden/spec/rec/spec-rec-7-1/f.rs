pub trait Kind { type Id; }
#[derive(Clone)]
pub struct GeneKind;
#[derive(Clone)]
pub struct ExonKind;
impl Kind for GeneKind { type Id = i64; }
impl Kind for ExonKind { type Id = i64; }

#[derive(Clone)]
pub struct LocT { pub pos: i64 }

#[derive(Clone)]
pub struct Edge<A: Kind<Id = i64>, B: Kind<Id = i64>> {
    pub src: <A as Kind>::Id,
    pub dst: <B as Kind>::Id,
}

pub fn same<A: Clone>(a: &A) -> A { a.clone() }
