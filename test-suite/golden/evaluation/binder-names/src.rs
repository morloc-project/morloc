use std::ffi::CString;
use std::sync::OnceLock;

use rustmorloc::arrow_schema::{DataType, Field, Schema};

static TABLE: OnceLock<i64> = OnceLock::new();

fn schema(n: i64) -> Schema {
    let fields: Vec<Field> = (0..n).map(|i| Field::new(format!("c{}", i), DataType::Int64, false)).collect();
    Schema::new(fields)
}

fn init_schemas() -> i64 {
    *TABLE.get_or_init(|| CString::new("ab").unwrap().as_bytes().len() as i64)
}

fn m1(x: i64) -> i64 {
    x
}

pub fn rs_count(n: i64) -> i64 {
    m1(schema(n).fields().len() as i64 + init_schemas())
}

pub struct SizeWalk;
pub struct PoolConfig;
fn read_variant_tag() -> i64 {
    0
}
fn local_dispatch() -> i64 {
    0
}
fn mlc_stage_lookup() -> i64 {
    0
}
mod mlc_user_ops {}

#[derive(Clone)]
pub struct Pt {
    pub a: i64,
    pub b: f64,
}

pub fn rs_pt(n: i64) -> Pt {
    Pt { a: n + read_variant_tag() + local_dispatch() + mlc_stage_lookup(), b: 0.5 }
}

pub fn rs_inc(n: i64) -> i64 {
    n + 1
}

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Dir {
    North = 0,
    South = 1,
}

pub fn rs_flip(d: Dir) -> Dir {
    match d {
        Dir::North => Dir::South,
        Dir::South => Dir::North,
    }
}
