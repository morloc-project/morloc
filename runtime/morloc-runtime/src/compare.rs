//! The order of two values under one schema (INTR-7, DERIV-3).
//!
//! Fields compare left to right; a `data` value by its constructor and then
//! its payload; an absent optional before a present one; sequences
//! lexicographically; strings by their UTF-8 bytes; integers by value;
//! floats by IEEE totalOrder. Two values compare equal exactly when every
//! float in them has the same bits.
use std::cmp::Ordering;

use crate::error::MorlocError;
use crate::recur::Resolver;
use crate::schema::{Schema, SerialType};
use crate::shm;
use crate::walk::{self, Frame, Stack, Walker};

/// The order of value `a` against value `b`, both laid out as `schema`
/// describes.
pub(crate) fn compare_voidstar_values(a: *const u8, b: *const u8, schema: &Schema) -> Result<Ordering, MorlocError> {
    let mut w = CmpWalk { res: Resolver::new(schema), ord: Ordering::Equal };
    let mut st = Stack::new();
    st.enter(schema, a, b);
    walk::run(&mut w, &mut st)?;
    Ok(w.ord)
}

/// Walks the two values in step. A frame's `data` is the node of `a`, its
/// `x` the same node of `b`. The first difference ends the walk.
struct CmpWalk<'r> {
    res: Resolver<'r>,
    ord: Ordering,
}

impl<'r> CmpWalk<'r> {
    /// Compare child `idx` now when nothing beneath it recurs, else leave it
    /// on the stack. Whether the parent may go on to its next child.
    fn child(
        &mut self,
        st: &mut Stack<*const u8>,
        f: &Frame<*const u8>,
        idx: usize,
        s: &'r Schema,
        a: *const u8,
        b: *const u8,
    ) -> Result<bool, MorlocError> {
        if self.res.flat(s) {
            self.step(st, Frame::new(s, a, b))?;
            Ok(self.ord == Ordering::Equal)
        } else {
            walk::defer(self, st, f, idx, s, a, b);
            Ok(false)
        }
    }

    fn decide(&mut self, st: &mut Stack<*const u8>, o: Ordering) {
        if o != Ordering::Equal {
            self.ord = o;
            st.clear();
        }
    }
}

unsafe fn read<T: Copy>(p: *const u8) -> T {
    std::ptr::read_unaligned(p as *const T)
}

unsafe fn bytes<'a>(p: *const u8) -> Result<&'a [u8], MorlocError> {
    let arr = read::<shm::Array>(p);
    if arr.size == 0 {
        return Ok(&[]);
    }
    Ok(std::slice::from_raw_parts(shm::rel2abs(arr.data)?, arr.size))
}

/// The two's-complement limbs of a variable-width integer, least
/// significant first.
unsafe fn limbs(p: *const u8) -> Result<Vec<u64>, MorlocError> {
    let size = read::<usize>(p);
    match size {
        0 => Ok(vec![0]),
        1 => Ok(vec![read::<i64>(p.add(8)) as u64]),
        n => {
            let data = shm::rel2abs(read::<shm::RelPtr>(p.add(8)))?;
            Ok((0..n).map(|i| read::<u64>(data.add(i * 8))).collect())
        }
    }
}

fn cmp_limbs(a: &[u64], b: &[u64]) -> Ordering {
    let sign = |x: &[u64]| if x.last().map_or(false, |l| (*l as i64) < 0) { u64::MAX } else { 0 };
    let (sa, sb) = (sign(a), sign(b));
    let n = a.len().max(b.len());
    let at = |x: &[u64], s: u64, i: usize| x.get(i).copied().unwrap_or(s);
    let top = (at(a, sa, n - 1) as i64).cmp(&(at(b, sb, n - 1) as i64));
    if top != Ordering::Equal {
        return top;
    }
    for i in (0..n - 1).rev() {
        let o = at(a, sa, i).cmp(&at(b, sb, i));
        if o != Ordering::Equal {
            return o;
        }
    }
    Ordering::Equal
}

/// An optional slot or a variant payload: absent before present.
unsafe fn present(p: *const u8) -> Result<Option<*const u8>, MorlocError> {
    let rel = read::<shm::RelPtr>(p);
    if rel == shm::RELNULL {
        Ok(None)
    } else {
        Ok(Some(shm::rel2abs(rel)? as *const u8))
    }
}

impl<'r> Walker<*const u8> for CmpWalk<'r> {
    fn step(&mut self, st: &mut Stack<*const u8>, f: Frame<*const u8>) -> Result<(), MorlocError> {
        // SAFETY: frames hold nodes of the tree the resolver was built from;
        // `data` and `x` point at two values laid out as that schema
        // describes, and every read is within the bounds it and the headers
        // give.
        let written: &'r Schema = unsafe { &*f.schema };
        let s = if written.serial_type == SerialType::Recur { self.res.resolve(written)? } else { written };
        let (a, b) = (f.data, f.x);
        unsafe {
            match s.serial_type {
                SerialType::Nil => {}
                SerialType::Bool => self.decide(st, (read::<u8>(a) != 0).cmp(&(read::<u8>(b) != 0))),
                SerialType::Enum | SerialType::Uint8 => self.decide(st, read::<u8>(a).cmp(&read::<u8>(b))),
                SerialType::Sint8 => self.decide(st, read::<i8>(a).cmp(&read::<i8>(b))),
                SerialType::Sint16 => self.decide(st, read::<i16>(a).cmp(&read::<i16>(b))),
                SerialType::Sint32 => self.decide(st, read::<i32>(a).cmp(&read::<i32>(b))),
                SerialType::Sint64 => self.decide(st, read::<i64>(a).cmp(&read::<i64>(b))),
                SerialType::Uint16 => self.decide(st, read::<u16>(a).cmp(&read::<u16>(b))),
                SerialType::Uint32 => self.decide(st, read::<u32>(a).cmp(&read::<u32>(b))),
                SerialType::Uint64 => self.decide(st, read::<u64>(a).cmp(&read::<u64>(b))),
                SerialType::Float32 => self.decide(st, read::<f32>(a).total_cmp(&read::<f32>(b))),
                SerialType::Float64 => self.decide(st, read::<f64>(a).total_cmp(&read::<f64>(b))),
                SerialType::Int => {
                    let o = cmp_limbs(&limbs(a)?, &limbs(b)?);
                    self.decide(st, o)
                }
                SerialType::String => {
                    let o = bytes(a)?.cmp(bytes(b)?);
                    self.decide(st, o)
                }
                SerialType::Array => {
                    let (xa, xb) = (read::<shm::Array>(a), read::<shm::Array>(b));
                    let n = xa.size.min(xb.size);
                    if n > 0 {
                        let elem = &s.parameters[0];
                        let (da, db) = (shm::rel2abs(xa.data)?, shm::rel2abs(xb.data)?);
                        for i in f.idx..n {
                            if !self.child(st, &f, i, elem, da.add(i * elem.width), db.add(i * elem.width))? {
                                return Ok(());
                            }
                        }
                    }
                    self.decide(st, xa.size.cmp(&xb.size))
                }
                SerialType::Tuple | SerialType::Map => {
                    for i in f.idx..s.parameters.len() {
                        let off = s.offsets[i];
                        if !self.child(st, &f, i, &s.parameters[i], a.add(off), b.add(off))? {
                            return Ok(());
                        }
                    }
                }
                SerialType::Optional => {
                    if f.idx > 0 {
                        return Ok(());
                    }
                    match (present(a)?, present(b)?) {
                        (Some(pa), Some(pb)) => {
                            self.child(st, &f, 0, &s.parameters[0], pa, pb)?;
                        }
                        (pa, pb) => self.decide(st, pa.is_some().cmp(&pb.is_some())),
                    }
                }
                SerialType::Variant => {
                    if f.idx > 0 {
                        return Ok(());
                    }
                    let (ta, tb) = (read::<u8>(a), read::<u8>(b));
                    if ta != tb {
                        self.decide(st, ta.cmp(&tb));
                        return Ok(());
                    }
                    let arm = s.parameters.get(ta as usize).ok_or_else(|| {
                        MorlocError::Serialization(format!("variant tag {} is out of range; the type has {} arms", ta, s.size))
                    })?;
                    match (present(a.add(8))?, present(b.add(8))?) {
                        (Some(pa), Some(pb)) => {
                            self.child(st, &f, 0, arm, pa, pb)?;
                        }
                        (pa, pb) => self.decide(st, pa.is_some().cmp(&pb.is_some())),
                    }
                }
                SerialType::Recur => {
                    return Err(MorlocError::Schema("back-reference resolves to a back-reference".to_string()));
                }
                SerialType::Table | SerialType::IFile | SerialType::OStream | SerialType::IStream => {
                    return Err(MorlocError::Other(format!(
                        "values of this type cannot be compared: {:?}",
                        s.serial_type
                    )));
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::json;
    use crate::schema::parse_schema;

    fn cmp(a: &str, b: &str, schema_str: &str) -> Ordering {
        let schema = parse_schema(schema_str).unwrap();
        let pa = json::read_json_with_schema(a, &schema).unwrap();
        let pb = json::read_json_with_schema(b, &schema).unwrap();
        compare_voidstar_values(pa as *const u8, pb as *const u8, &schema).unwrap()
    }

    #[test]
    fn scalars_compare_by_value() {
        let _shm = crate::init_test_shm();
        assert_eq!(cmp("-3", "2", "i4"), Ordering::Less);
        assert_eq!(cmp("7", "7", "u8"), Ordering::Equal);
        assert_eq!(cmp("true", "false", "b"), Ordering::Greater);
        assert_eq!(cmp("-1", "1", "j"), Ordering::Less);
        assert_eq!(cmp("123456789012345678901234567890", "5", "j"), Ordering::Greater);
        assert_eq!(cmp("-123456789012345678901234567890", "-5", "j"), Ordering::Less);
        assert_eq!(cmp("123456789012345678901234567890", "123456789012345678901234567890", "j"), Ordering::Equal);
    }

    #[test]
    fn floats_use_total_order_and_bitwise_equality() {
        let _shm = crate::init_test_shm();
        assert_eq!(cmp("-0.0", "0.0", "f8"), Ordering::Less);
        assert_eq!(cmp("1.5", "1.5", "f8"), Ordering::Equal);
        assert_eq!(cmp("1.5", "2.5", "f4"), Ordering::Less);
    }

    #[test]
    fn strings_and_lists_are_lexicographic() {
        let _shm = crate::init_test_shm();
        assert_eq!(cmp("\"ab\"", "\"b\"", "s"), Ordering::Less);
        assert_eq!(cmp("\"ab\"", "\"a\"", "s"), Ordering::Greater);
        assert_eq!(cmp("\"\"", "\"\"", "s"), Ordering::Equal);
        assert_eq!(cmp("[1,2]", "[1,2,0]", "ai4"), Ordering::Less);
        assert_eq!(cmp("[1,3]", "[1,2,9]", "ai4"), Ordering::Greater);
        assert_eq!(cmp("[]", "[]", "ai4"), Ordering::Equal);
        assert_eq!(cmp("[\"x\",\"y\"]", "[\"x\",\"y\"]", "as"), Ordering::Equal);
    }

    #[test]
    fn tuples_compare_field_by_field() {
        let _shm = crate::init_test_shm();
        assert_eq!(cmp("[1,\"b\"]", "[1,\"a\"]", "t2i4s"), Ordering::Greater);
        assert_eq!(cmp("[0,\"z\"]", "[1,\"a\"]", "t2i4s"), Ordering::Less);
        assert_eq!(cmp("[2,\"a\"]", "[2,\"a\"]", "t2i4s"), Ordering::Equal);
    }

    #[test]
    fn absent_sorts_before_present() {
        let _shm = crate::init_test_shm();
        assert_eq!(cmp("null", "0", "?i4"), Ordering::Less);
        assert_eq!(cmp("null", "null", "?i4"), Ordering::Equal);
        assert_eq!(cmp("4", "3", "?i4"), Ordering::Greater);
    }

    #[test]
    fn data_compares_constructor_then_payload() {
        let _shm = crate::init_test_shm();
        let s = "v36Circle1f84Rect2f8f83Dot0";
        assert_eq!(cmp("{\"Circle\":[1.0]}", "{\"Rect\":[0.5,0.5]}", s), Ordering::Less);
        assert_eq!(cmp("{\"Rect\":[1.0,2.0]}", "{\"Rect\":[1.0,1.0]}", s), Ordering::Greater);
        assert_eq!(cmp("\"Dot\"", "\"Dot\"", s), Ordering::Equal);
        assert_eq!(cmp("{\"Circle\":[2.0]}", "\"Dot\"", s), Ordering::Less);
    }

    #[test]
    fn recursive_values_compare_to_the_bottom() {
        let _shm = crate::init_test_shm();
        let s = "&2LLm24heads4tail?^2LL";
        let list = |xs: &[&str]| {
            let mut t = "null".to_string();
            for x in xs.iter().rev() {
                t = format!("{{\"head\":\"{}\",\"tail\":{}}}", x, t);
            }
            t
        };
        let long: Vec<String> = (0..200).map(|i| format!("x{}", i)).collect();
        let long: Vec<&str> = long.iter().map(|x| x.as_str()).collect();
        assert_eq!(cmp(&list(&long), &list(&long), s), Ordering::Equal);
        assert_eq!(cmp(&list(&["a", "b"]), &list(&["a", "c"]), s), Ordering::Less);
        assert_eq!(cmp(&list(&["a", "b"]), &list(&["a"]), s), Ordering::Greater);
    }

    #[test]
    fn equal_values_hash_alike() {
        let _shm = crate::init_test_shm();
        let cases = [
            ("[1,2.5]", "t2i1f8"),
            ("[[3,\"a\"],[4,\"b\"]]", "at2u1s"),
            ("{\"Rect\":[1.0,2.0]}", "v36Circle1f84Rect2f8f83Dot0"),
            ("123456789012345678901234567890", "j"),
            ("0", "j"),
        ];
        for (v, sch) in cases {
            let schema = parse_schema(sch).unwrap();
            let pa = json::read_json_with_schema(v, &schema).unwrap();
            let pb = json::read_json_with_schema(v, &schema).unwrap();
            assert_eq!(compare_voidstar_values(pa as *const u8, pb as *const u8, &schema).unwrap(), Ordering::Equal);
            let ha = crate::cache::hash_voidstar_value(pa as *const u8, &schema, 0).unwrap();
            let hb = crate::cache::hash_voidstar_value(pb as *const u8, &schema, 0).unwrap();
            assert_eq!(ha, hb, "{} as {}", v, sch);
        }
    }
}
