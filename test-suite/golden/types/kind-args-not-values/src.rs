use std::sync::Arc;

use rustmorloc::arrow_array::cast::AsArray;
use rustmorloc::arrow_array::types::Int64Type;
use rustmorloc::arrow_array::{ArrayRef, Int64Array, RecordBatch, StringArray};
use rustmorloc::arrow_schema::{DataType, Field, Schema as ArrowSchema};

#[derive(Clone)]
pub struct P {
    pub a: i64,
    pub b: f64,
}

pub fn mk(n: i64) -> RecordBatch {
    let schema = Arc::new(ArrowSchema::new(vec![
        Field::new("x", DataType::Int64, false),
        Field::new("y", DataType::Utf8, false),
    ]));
    let xs: ArrayRef = Arc::new(Int64Array::from((0..n).collect::<Vec<i64>>()));
    let ys: ArrayRef = Arc::new(StringArray::from((0..n).map(|i| i.to_string()).collect::<Vec<String>>()));
    RecordBatch::try_new(schema, vec![xs, ys]).unwrap()
}

pub fn describe(t: &RecordBatch) -> String {
    let xs: Vec<String> = t.column(0).as_primitive::<Int64Type>().values().iter().map(|x| x.to_string()).collect();
    let ys: Vec<String> = t.column(1).as_string::<i32>().iter().map(|y| format!("'{}'", y.unwrap())).collect();
    format!("{} rows x=[{}] y=[{}]", t.num_rows(), xs.join(", "), ys.join(", "))
}

pub fn nrow(t: &RecordBatch) -> i64 {
    t.num_rows() as i64
}

pub fn points(n: i64) -> Vec<P> {
    (0..n).map(|i| P { a: i, b: 0.5 }).collect()
}

pub fn point(n: i64) -> P {
    P { a: n, b: 0.5 }
}
