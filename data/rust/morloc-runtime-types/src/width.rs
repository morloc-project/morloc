//! Conversions between the integer widths the runtime mixes: `usize`
//! lengths, `u64` wire and file sizes, `isize` relative pointers and `i64`
//! morloc integers.
//!
//! The conversions here are lossless because the runtime supports only
//! 64-bit targets, which the assertion below makes a build error rather than
//! an assumption. A conversion that can lose information on a supported
//! target does not belong here; it goes through `TryFrom` at its use.

#[cfg(not(target_pointer_width = "64"))]
compile_error!(
    "morloc supports only 64-bit targets: a relative pointer packs a volume \
     index and an offset into 64 bits"
);

use crate::error::MorlocError;
use crate::schema::SerialType;

#[allow(clippy::cast_possible_truncation)]
#[inline]
pub const fn usize_from_u64(n: u64) -> usize {
    n as usize
}

#[inline]
pub const fn u64_from_usize(n: usize) -> u64 {
    n as u64
}

#[allow(clippy::cast_possible_truncation)]
#[inline]
pub const fn isize_from_i64(n: i64) -> isize {
    n as isize
}

#[inline]
pub const fn i64_from_isize(n: isize) -> i64 {
    n as i64
}

/// The low and high 64 bits of `w`, for multi-limb arithmetic.
#[allow(clippy::cast_possible_truncation)]
#[inline]
pub const fn split_u128(w: u128) -> (u64, u64) {
    (w as u64, (w >> 64) as u64)
}

/// A fixed-width integer slot. Its range is the type's own, so the bound a
/// value is checked against and the conversion made are one fact.
pub trait IntSlot: TryFrom<i128> + std::str::FromStr<Err = std::num::ParseIntError> {
    const LO: i128;
    const HI: i128;
    /// The morloc name of the slot type, for messages.
    const NAME: &'static str;
}

macro_rules! int_slot {
    ($($t:ty => $name:literal),*) => {
        $(impl IntSlot for $t {
            const LO: i128 = <$t>::MIN as i128;
            const HI: i128 = <$t>::MAX as i128;
            const NAME: &'static str = $name;
        })*
    };
}
int_slot!(i8 => "I8", i16 => "I16", i32 => "I32", i64 => "I64",
          u8 => "U8", u16 => "U16", u32 => "U32", u64 => "U64");

/// `v` in slot type `T`, or an error naming the slot and its range.
pub fn narrow_int<T: IntSlot>(v: i128) -> Result<T, MorlocError> {
    T::try_from(v).map_err(|_| out_of_range::<T>(&v))
}

/// The error for a value (shown as given) outside slot type `T`.
pub fn out_of_range<T: IntSlot>(shown: &dyn std::fmt::Display) -> MorlocError {
    MorlocError::Serialization(format!(
        "value {} out of range for {} (range {} to {})", shown, T::NAME, T::LO, T::HI
    ))
}

/// Write `v` at `dest` in the fixed-width integer slot `kind`, at the slot's
/// own width; a value the slot cannot hold is an error, never a truncation.
/// `None` when `kind` is not a fixed-width integer.
///
/// # Safety
///
/// `dest` must be writable for the slot's width.
pub unsafe fn write_int_slot(kind: SerialType, dest: *mut u8, v: i128) -> Option<Result<(), MorlocError>> {
    unsafe fn put<T: IntSlot>(dest: *mut u8, v: i128) -> Result<(), MorlocError> {
        std::ptr::write_unaligned(dest as *mut T, narrow_int::<T>(v)?);
        Ok(())
    }
    Some(match kind {
        SerialType::Sint8 => put::<i8>(dest, v),
        SerialType::Sint16 => put::<i16>(dest, v),
        SerialType::Sint32 => put::<i32>(dest, v),
        SerialType::Sint64 => put::<i64>(dest, v),
        SerialType::Uint8 => put::<u8>(dest, v),
        SerialType::Uint16 => put::<u16>(dest, v),
        SerialType::Uint32 => put::<u32>(dest, v),
        SerialType::Uint64 => put::<u64>(dest, v),
        SerialType::Nil | SerialType::Bool | SerialType::Float32 | SerialType::Float64
        | SerialType::String | SerialType::Array | SerialType::Tuple | SerialType::Map
        | SerialType::Optional | SerialType::Int | SerialType::Table | SerialType::Recur
        | SerialType::IFile | SerialType::OStream | SerialType::IStream | SerialType::Variant
        | SerialType::Enum => return None,
    })
}

/// The one-byte tag of arm `position` of a sum type with `n_arms` arms, or
/// `None` when no arm is there. Schema parsing limits a sum type to 256 arms,
/// so every arm has a tag.
pub fn arm_tag<P: TryInto<u8>>(position: P, n_arms: usize) -> Option<u8> {
    position.try_into().ok().filter(|&t| usize::from(t) < n_arms)
}

/// The `f32` nearest to `v`, or an error when a finite `v` lies beyond the
/// `f32` range and would become infinite. Rounding is the nature of the
/// narrower type; overflow is not.
#[allow(clippy::cast_possible_truncation)]
#[inline]
pub fn f32_nearest(v: f64) -> Result<f32, MorlocError> {
    let f = v as f32;
    if f.is_infinite() && v.is_finite() {
        Err(MorlocError::Serialization(format!("value {} out of range for F32", v)))
    } else {
        Ok(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trips_at_the_extremes() {
        for n in [0u64, 1, u64::MAX] {
            assert_eq!(u64_from_usize(usize_from_u64(n)), n);
        }
        for n in [0i64, -1, i64::MIN, i64::MAX] {
            assert_eq!(i64_from_isize(isize_from_i64(n)), n);
        }
    }

    #[test]
    fn split_u128_halves() {
        assert_eq!(split_u128(u128::MAX), (u64::MAX, u64::MAX));
        assert_eq!(split_u128(1u128 << 64 | 7), (7, 1));
    }

    #[test]
    fn f32_rejects_only_overflow() {
        assert_eq!(f32_nearest(0.1).unwrap(), 0.1f32);
        assert_eq!(f32_nearest(f64::from(f32::MAX)).unwrap(), f32::MAX);
        assert_eq!(f32_nearest(f64::INFINITY).unwrap(), f32::INFINITY);
        assert!(f32_nearest(f64::NAN).unwrap().is_nan());
        assert!(f32_nearest(1e39).is_err());
        assert!(f32_nearest(-1e39).is_err());
    }
}
