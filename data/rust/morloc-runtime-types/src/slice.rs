//! Python index and slice semantics over a sequence of `n` elements, shared
//! by every bracket accessor so that they select the same elements.
//!
//! Positions are computed in `i128`: a normalised position lies in
//! `[-1, n]` and a step in the `i64` range, so no intermediate can overflow,
//! whatever the step.

use crate::error::MorlocError;
use crate::width::{u64_from_usize, usize_from_u64};

/// The position `idx` names in a sequence of `n` elements, counting from
/// the end when negative, or `None` when it is out of bounds.
pub fn resolve_index(idx: i64, n: u64) -> Option<u64> {
    let n = i128::from(n);
    let i = i128::from(idx);
    let at = if i < 0 { i + n } else { i };
    if (0..n).contains(&at) { u64::try_from(at).ok() } else { None }
}

/// `resolve_index` over an in-memory array of `n` elements.
pub fn resolve_array_index(idx: i64, n: usize) -> Option<usize> {
    resolve_index(idx, u64_from_usize(n)).map(usize_from_u64)
}

/// A slice `[start:stop:step]` resolved against a sequence of `n`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Slice {
    start: i128,
    stop: i128,
    step: i128,
}

impl Slice {
    pub fn new(
        n: u64,
        start: Option<i64>,
        stop: Option<i64>,
        step: Option<i64>,
    ) -> Result<Self, MorlocError> {
        let step = i128::from(step.unwrap_or(1));
        if step == 0 {
            return Err(MorlocError::Other("Bracket slice step cannot be 0".into()));
        }
        let n = i128::from(n);
        // A negative bound counts from the end; the result is clamped to
        // the range the step walks, where -1 is "before the first element"
        // for a backward slice.
        let bound = |v: i64| {
            let v = i128::from(v);
            let v = if v < 0 { v + n } else { v };
            if step > 0 { v.clamp(0, n) } else { v.clamp(-1, n - 1) }
        };
        let (start_default, stop_default) = if step > 0 { (0, n) } else { (n - 1, -1) };
        Ok(Slice {
            start: start.map_or(start_default, bound),
            stop: stop.map_or(stop_default, bound),
            step,
        })
    }

    /// `Slice::new` over an in-memory array of `n` elements.
    pub fn over_array(
        n: usize,
        start: Option<i64>,
        stop: Option<i64>,
        step: Option<i64>,
    ) -> Result<Self, MorlocError> {
        Self::new(u64_from_usize(n), start, stop, step)
    }

    /// `[lo, hi)` when the slice is a non-empty forward run with step 1.
    pub fn unit_range(&self) -> Option<(u64, u64)> {
        if self.step == 1 && self.start < self.stop {
            Some((u64::try_from(self.start).ok()?, u64::try_from(self.stop).ok()?))
        } else {
            None
        }
    }

    /// The number of elements the slice selects.
    pub fn len(&self) -> u64 {
        let span = if self.step > 0 { self.stop - self.start } else { self.start - self.stop };
        if span <= 0 {
            return 0;
        }
        let step = self.step.abs();
        u64::try_from((span + step - 1) / step).unwrap_or(u64::MAX)
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// The selected positions in an in-memory array, in slice order.
    pub fn positions(self) -> impl Iterator<Item = usize> {
        self.indices().map(usize_from_u64)
    }

    /// The selected positions, in slice order.
    pub fn indices(self) -> impl Iterator<Item = u64> {
        let mut i = self.start;
        std::iter::from_fn(move || {
            let inside = if self.step > 0 { i < self.stop } else { i > self.stop };
            if !inside {
                return None;
            }
            let at = u64::try_from(i).ok()?;
            i += self.step;
            Some(at)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sel(n: u64, a: Option<i64>, b: Option<i64>, c: Option<i64>) -> Vec<u64> {
        Slice::new(n, a, b, c).unwrap().indices().collect()
    }

    #[test]
    fn index_counts_from_either_end() {
        assert_eq!(resolve_index(0, 3), Some(0));
        assert_eq!(resolve_index(-1, 3), Some(2));
        assert_eq!(resolve_index(-3, 3), Some(0));
        assert_eq!(resolve_index(3, 3), None);
        assert_eq!(resolve_index(-4, 3), None);
        assert_eq!(resolve_index(i64::MIN, 3), None);
        assert_eq!(resolve_index(0, 0), None);
    }

    #[test]
    fn slices_follow_python() {
        assert_eq!(sel(5, None, None, None), vec![0, 1, 2, 3, 4]);
        assert_eq!(sel(5, Some(1), Some(-1), None), vec![1, 2, 3]);
        assert_eq!(sel(5, None, None, Some(-1)), vec![4, 3, 2, 1, 0]);
        assert_eq!(sel(5, None, None, Some(2)), vec![0, 2, 4]);
        assert_eq!(sel(5, Some(-2), None, Some(-2)), vec![3, 1]);
        assert_eq!(sel(5, Some(10), None, None), Vec::<u64>::new());
        assert_eq!(sel(5, Some(-10), Some(10), None), vec![0, 1, 2, 3, 4]);
        assert_eq!(sel(0, None, None, Some(-1)), Vec::<u64>::new());
    }

    #[test]
    fn a_huge_step_selects_one_element_and_stops() {
        assert_eq!(sel(5, Some(1), None, Some(i64::MAX)), vec![1]);
        assert_eq!(sel(5, Some(3), None, Some(i64::MIN)), vec![3]);
        assert_eq!(sel(u64::MAX, Some(i64::MAX), None, Some(i64::MAX)).len(), 2);
    }

    #[test]
    fn len_matches_the_indices() {
        for n in 0..7u64 {
            for step in [-3i64, -2, -1, 1, 2, 3, i64::MAX, i64::MIN] {
                for a in [None, Some(-8), Some(-1), Some(0), Some(2), Some(9)] {
                    for b in [None, Some(-8), Some(-1), Some(0), Some(4), Some(9)] {
                        let s = Slice::new(n, a, b, Some(step)).unwrap();
                        assert_eq!(s.len(), s.indices().count() as u64, "{n} {a:?} {b:?} {step}");
                    }
                }
            }
        }
    }

    #[test]
    fn step_zero_is_an_error() {
        assert!(Slice::new(3, None, None, Some(0)).is_err());
    }
}
