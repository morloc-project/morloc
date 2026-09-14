// Canonical Arrow table fixture and describer, Rust side. Mirrors src.py:
// same columns, same values, same ASCII description.

use std::sync::Arc;

use rustmorloc::arrow_array::builder::{Int64Builder, ListBuilder};
use rustmorloc::arrow_array::cast::AsArray;
use rustmorloc::arrow_array::types::*;
use rustmorloc::arrow_array::{
    Array, ArrayRef, BinaryArray, BooleanArray, Date32Array, DurationMicrosecondArray, Float32Array,
    Float64Array, Int16Array, Int32Array, Int64Array, Int8Array, LargeStringArray, RecordBatch,
    StringArray, TimestampMicrosecondArray, UInt16Array, UInt32Array, UInt64Array, UInt8Array,
};
use rustmorloc::arrow_schema::{DataType, Field, Schema as ArrowSchema, TimeUnit};

fn take<T: Clone>(v: &[Option<T>], case: &str) -> Vec<Option<T>> {
    match case {
        "full" => v.to_vec(),
        "one" => v[..1].to_vec(),
        "empty" => vec![],
        "allnull" => vec![None; 5],
        other => panic!("unknown case {}", other),
    }
}

pub fn mk(case: &String) -> RecordBatch {
    let c = case.as_str();
    let b = BooleanArray::from(take(&[Some(true), None, Some(false), Some(true), Some(false)], c));
    let i8_ = Int8Array::from(take(&[Some(-128i8), None, Some(0), Some(127), Some(1)], c));
    let i16_ = Int16Array::from(take(&[Some(-32768i16), None, Some(0), Some(32767), Some(2)], c));
    let i32_ = Int32Array::from(take(&[Some(-2147483648i32), None, Some(0), Some(2147483647), Some(3)], c));
    let i64_ = Int64Array::from(take(&[Some(i64::MIN), None, Some(0), Some(i64::MAX), Some(4)], c));
    let u8_ = UInt8Array::from(take(&[Some(0u8), None, Some(255), Some(7), Some(5)], c));
    let u16_ = UInt16Array::from(take(&[Some(0u16), None, Some(65535), Some(7), Some(6)], c));
    let u32_ = UInt32Array::from(take(&[Some(0u32), None, Some(4294967295), Some(7), Some(7)], c));
    let u64_ = UInt64Array::from(take(&[Some(0u64), None, Some(u64::MAX), Some(7), Some(8)], c));
    let f32_ = Float32Array::from(take(&[Some(0.5f32), None, Some(-1.5), Some(100.125), Some(3.0)], c));
    let f64_ = Float64Array::from(take(&[Some(0.5f64), None, Some(-1.5), Some(100.125), Some(0.001)], c));
    let s = StringArray::from(take(&[Some(""), None, Some("abc"), Some("h\u{e9}llo"), Some("x")], c));
    let ls = LargeStringArray::from(take(&[Some(""), None, Some("abc"), Some("\u{65e5}\u{672c}"), Some("y")], c));
    let bin = BinaryArray::from(take(
        &[Some(&b""[..]), None, Some(&b"\x00\x01"[..]), Some(&b"ab"[..]), Some(&b"\xff"[..])],
        c,
    ));
    let d = Date32Array::from(take(&[Some(0i32), None, Some(18262), Some(-1), Some(1)], c));
    let ts = TimestampMicrosecondArray::from(take(&[Some(0i64), None, Some(1577836800000000), Some(-1), Some(1)], c))
        .with_timezone("UTC");
    let dur = DurationMicrosecondArray::from(take(&[Some(0i64), None, Some(86400000000), Some(-1), Some(1)], c));
    let mut lb = ListBuilder::new(Int64Builder::new());
    let lists: [Option<Vec<i64>>; 5] = [Some(vec![]), None, Some(vec![1, 2, 3]), Some(vec![4]), Some(vec![5, 6])];
    for item in take(&lists, c) {
        match item {
            Some(v) => lb.append_value(v.into_iter().map(Some)),
            None => lb.append_null(),
        }
    }
    let li = lb.finish();

    let fields = vec![
        Field::new("b", DataType::Boolean, true),
        Field::new("i8", DataType::Int8, true),
        Field::new("i16", DataType::Int16, true),
        Field::new("i32", DataType::Int32, true),
        Field::new("i64", DataType::Int64, true),
        Field::new("u8", DataType::UInt8, true),
        Field::new("u16", DataType::UInt16, true),
        Field::new("u32", DataType::UInt32, true),
        Field::new("u64", DataType::UInt64, true),
        Field::new("f32", DataType::Float32, true),
        Field::new("f64", DataType::Float64, true),
        Field::new("s", DataType::Utf8, true),
        Field::new("ls", DataType::LargeUtf8, true),
        Field::new("bin", DataType::Binary, true),
        Field::new("d", DataType::Date32, true),
        Field::new("ts", DataType::Timestamp(TimeUnit::Microsecond, Some("UTC".into())), true),
        Field::new("dur", DataType::Duration(TimeUnit::Microsecond), true),
        Field::new("li", DataType::List(Arc::new(Field::new("item", DataType::Int64, true))), true),
    ];
    let columns: Vec<ArrayRef> = vec![
        Arc::new(b), Arc::new(i8_), Arc::new(i16_), Arc::new(i32_), Arc::new(i64_),
        Arc::new(u8_), Arc::new(u16_), Arc::new(u32_), Arc::new(u64_),
        Arc::new(f32_), Arc::new(f64_), Arc::new(s), Arc::new(ls), Arc::new(bin),
        Arc::new(d), Arc::new(ts), Arc::new(dur), Arc::new(li),
    ];
    RecordBatch::try_new(Arc::new(ArrowSchema::new(fields)), columns).expect("fixture")
}

fn esc(bytes: &[u8]) -> String {
    let mut out = String::new();
    for &c in bytes {
        if (0x20..=0x7e).contains(&c) && c != 0x5c {
            out.push(c as char);
        } else {
            out.push_str(&format!("\\x{:02x}", c));
        }
    }
    out
}

fn tname(dt: &DataType) -> String {
    match dt {
        DataType::Boolean => "bool".into(),
        DataType::Int8 => "i8".into(),
        DataType::Int16 => "i16".into(),
        DataType::Int32 => "i32".into(),
        DataType::Int64 => "i64".into(),
        DataType::UInt8 => "u8".into(),
        DataType::UInt16 => "u16".into(),
        DataType::UInt32 => "u32".into(),
        DataType::UInt64 => "u64".into(),
        DataType::Float32 => "f32".into(),
        DataType::Float64 => "f64".into(),
        DataType::Utf8 => "utf8".into(),
        DataType::LargeUtf8 => "large_utf8".into(),
        DataType::Binary => "binary".into(),
        DataType::Date32 => "date32".into(),
        DataType::Timestamp(TimeUnit::Microsecond, Some(tz)) => format!("ts_us_{}", tz),
        DataType::Duration(TimeUnit::Microsecond) => "dur_us".into(),
        DataType::List(f) => format!("list_{}", tname(f.data_type())),
        other => format!("?{}", other),
    }
}

// C's %g for the values the fixture uses.
fn g(v: f64) -> String {
    if v == v.trunc() && v.abs() < 1e15 {
        format!("{}", v as i64)
    } else {
        let s = format!("{}", v);
        s
    }
}

fn cell(col: &dyn Array, i: usize) -> String {
    if col.is_null(i) {
        return "null".into();
    }
    match col.data_type() {
        DataType::Boolean => if col.as_boolean().value(i) { "true".into() } else { "false".into() },
        DataType::Int8 => col.as_primitive::<Int8Type>().value(i).to_string(),
        DataType::Int16 => col.as_primitive::<Int16Type>().value(i).to_string(),
        DataType::Int32 => col.as_primitive::<Int32Type>().value(i).to_string(),
        DataType::Int64 => col.as_primitive::<Int64Type>().value(i).to_string(),
        DataType::UInt8 => col.as_primitive::<UInt8Type>().value(i).to_string(),
        DataType::UInt16 => col.as_primitive::<UInt16Type>().value(i).to_string(),
        DataType::UInt32 => col.as_primitive::<UInt32Type>().value(i).to_string(),
        DataType::UInt64 => col.as_primitive::<UInt64Type>().value(i).to_string(),
        DataType::Float32 => g(col.as_primitive::<Float32Type>().value(i) as f64),
        DataType::Float64 => g(col.as_primitive::<Float64Type>().value(i)),
        DataType::Utf8 => esc(col.as_string::<i32>().value(i).as_bytes()),
        DataType::LargeUtf8 => esc(col.as_string::<i64>().value(i).as_bytes()),
        DataType::Binary => esc(col.as_binary::<i32>().value(i)),
        DataType::Date32 => col.as_primitive::<Date32Type>().value(i).to_string(),
        DataType::Timestamp(TimeUnit::Microsecond, _) => col.as_primitive::<TimestampMicrosecondType>().value(i).to_string(),
        DataType::Duration(TimeUnit::Microsecond) => col.as_primitive::<DurationMicrosecondType>().value(i).to_string(),
        DataType::List(_) => {
            let inner = col.as_list::<i32>().value(i);
            let items: Vec<String> = (0..inner.len()).map(|k| cell(inner.as_ref(), k)).collect();
            format!("[{}]", items.join(","))
        }
        _ => "?".into(),
    }
}

pub fn describe(t: &RecordBatch) -> String {
    let mut lines = vec![format!("rows={} cols={}", t.num_rows(), t.num_columns())];
    for (field, col) in t.schema().fields().iter().zip(t.columns()) {
        let vals: Vec<String> = (0..col.len()).map(|i| cell(col.as_ref(), i)).collect();
        lines.push(format!(
            "{} {} n={} nulls={} [{}]",
            field.name(),
            tname(field.data_type()),
            col.len(),
            col.null_count(),
            vals.join(",")
        ));
    }
    lines.join("\n")
}
