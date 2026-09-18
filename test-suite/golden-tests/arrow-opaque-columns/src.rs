use rustmorloc::arrow_array::cast::AsArray;
use rustmorloc::arrow_array::types::Decimal128Type;
use rustmorloc::arrow_array::{Array, RecordBatch};

pub fn price(t: &RecordBatch) -> String {
    let col = t.column(1).as_primitive::<Decimal128Type>();
    format!("{} {}", col.value_as_string(0), col.data_type())
}

pub fn label(t: &RecordBatch) -> String {
    let col = t.column(1).as_string_view();
    format!("{} {}", col.value(0), col.data_type())
}
