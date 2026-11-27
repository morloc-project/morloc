//! The body copy of a leaf element written in place (SLOT-36): a `Str`'s
//! bytes checked as UTF-8 (FMT-10) while they are copied, or an array's
//! fixed-width elements copied as they are.

/// Bodies below this are checked whole and copied with one memcpy.
pub(crate) const STREAM_MIN: usize = 256 << 10;

/// The chunk a large body is checked and copied in, so that the bytes the
/// check reads are still in cache for the copy.
pub(crate) const CHUNK: usize = 256 << 10;

/// Copy `n` bytes from `src` to `dst`, checking them as UTF-8 first when
/// `check`. On a byte that is not UTF-8, the offset of the first one; the
/// bytes before it may have been copied. A large body is copied with
/// streaming stores only when `streaming`: a body this process compresses
/// next is better left in cache.
///
/// # Safety
/// `src` addresses `n` readable bytes and `dst` `n` writable ones; the two
/// do not overlap.
pub(crate) unsafe fn copy_leaf(dst: *mut u8, src: *const u8, n: usize, check: bool, streaming: bool) -> Result<(), usize> {
    copy_leaf_with(dst, src, n, check, CHUNK, if streaming { STREAM_MIN } else { usize::MAX })
}

/// `copy_leaf` with the chunk size and the size at which the streaming
/// copy starts as parameters, for tests.
///
/// # Safety
/// As `copy_leaf`.
pub(crate) unsafe fn copy_leaf_with(
    dst: *mut u8,
    src: *const u8,
    n: usize,
    check: bool,
    chunk: usize,
    stream_min: usize,
) -> Result<(), usize> {
    if n == 0 {
        return Ok(());
    }
    debug_assert!(
        (dst as usize).saturating_add(n) <= src as usize || (src as usize).saturating_add(n) <= dst as usize,
        "a leaf's source overlaps its room"
    );
    let bytes = std::slice::from_raw_parts(src, n);
    if !cfg!(target_arch = "x86_64") || n < stream_min {
        if check {
            if let Some(i) = morloc_runtime_types::utf8::first_invalid(bytes) {
                return Err(i);
            }
        }
        std::ptr::copy_nonoverlapping(src, dst, n);
        return Ok(());
    }
    let _fence = Fence;
    let chunk = chunk.max(4);
    let mut a = 0;
    while a < n {
        let mut b = (a + chunk).min(n);
        if check {
            b = checked_end(&bytes[a..b], b == n).map_err(|i| a + i)? + a;
        }
        stream_copy(dst.add(a), src.add(a), b - a);
        a = b;
    }
    Ok(())
}

/// Where a checked chunk ends: all of `part` when it is valid UTF-8, or,
/// when it ends inside a sequence that the next chunk may complete (not
/// `last`), the start of that sequence. Otherwise the offset of the first
/// byte that is not UTF-8.
fn checked_end(part: &[u8], last: bool) -> Result<usize, usize> {
    match morloc_runtime_types::utf8::check_part(part) {
        Ok(()) => Ok(part.len()),
        Err((at, incomplete)) if !last && incomplete && at + 3 >= part.len() && at > 0 => Ok(at),
        Err((at, _)) => Err(at),
    }
}

/// SHM-6, SLOT-36: an sfence on every way out of the copy.
struct Fence;

impl Drop for Fence {
    fn drop(&mut self) {
        #[cfg(target_arch = "x86_64")]
        // SAFETY: sfence has no operands.
        unsafe {
            std::arch::x86_64::_mm_sfence()
        };
    }
}

/// Copy `n` bytes with streaming stores for the 16-aligned 64-byte runs of
/// the destination and plain copies around them.
///
/// # Safety
/// As `copy_leaf`.
#[cfg(target_arch = "x86_64")]
unsafe fn stream_copy(dst: *mut u8, src: *const u8, n: usize) {
    use std::arch::x86_64::{__m128i, _mm_loadu_si128, _mm_stream_si128};
    let head = dst.align_offset(16).min(n);
    std::ptr::copy_nonoverlapping(src, dst, head);
    let body = (n - head) & !63;
    let (mut d, mut s) = (dst.add(head), src.add(head));
    let end = d.add(body);
    while d < end {
        let x0 = _mm_loadu_si128(s as *const __m128i);
        let x1 = _mm_loadu_si128(s.add(16) as *const __m128i);
        let x2 = _mm_loadu_si128(s.add(32) as *const __m128i);
        let x3 = _mm_loadu_si128(s.add(48) as *const __m128i);
        _mm_stream_si128(d as *mut __m128i, x0);
        _mm_stream_si128(d.add(16) as *mut __m128i, x1);
        _mm_stream_si128(d.add(32) as *mut __m128i, x2);
        _mm_stream_si128(d.add(48) as *mut __m128i, x3);
        d = d.add(64);
        s = s.add(64);
    }
    std::ptr::copy_nonoverlapping(s, d, n - head - body);
}

#[cfg(not(target_arch = "x86_64"))]
unsafe fn stream_copy(dst: *mut u8, src: *const u8, n: usize) {
    std::ptr::copy_nonoverlapping(src, dst, n);
}

#[cfg(test)]
mod tests {
    use super::copy_leaf_with;

    fn copied(src: &[u8], check: bool, chunk: usize, stream_min: usize) -> Result<Vec<u8>, usize> {
        let mut dst = vec![0xEEu8; src.len() + 3];
        unsafe { copy_leaf_with(dst.as_mut_ptr().add(1), src.as_ptr(), src.len(), check, chunk, stream_min) }?;
        assert_eq!(dst[0], 0xEE);
        assert_eq!(&dst[src.len() + 1..], &[0xEE, 0xEE]);
        Ok(dst[1..src.len() + 1].to_vec())
    }

    fn reference(src: &[u8]) -> Result<Vec<u8>, usize> {
        match morloc_runtime_types::utf8::first_invalid(src) {
            Some(i) => Err(i),
            None => Ok(src.to_vec()),
        }
    }

    // FMT-10, SLOT-36
    #[test]
    fn a_character_split_across_any_chunk_boundary_is_copied() {
        for ch in ["\u{e9}", "\u{4e2d}", "\u{1F600}"] {
            for pre in 0..80 {
                let mut src = vec![b'x'; pre];
                for _ in 0..40 {
                    src.extend_from_slice(ch.as_bytes());
                }
                src.extend_from_slice(b"tail");
                for chunk in [4, 5, 7, 16, 64] {
                    for stream_min in [0, usize::MAX] {
                        assert_eq!(copied(&src, true, chunk, stream_min), Ok(src.clone()), "{ch} pre {pre} chunk {chunk}");
                    }
                }
            }
        }
    }

    // FMT-10, SLOT-36
    #[test]
    fn a_byte_that_is_not_utf8_is_found_at_its_offset_in_any_chunk() {
        let base: Vec<u8> = "ab\u{e9}cd\u{1F600}ef\u{4e2d}gh".repeat(30).into_bytes();
        let bad_runs: [&[u8]; 6] = [b"\xff", b"\x80", b"\xe2\x82", b"\xf0\x9f\x98", b"\xed\xa0\x80", b"\xc0\x80"];
        for at in 0..base.len() {
            for run in bad_runs {
                let mut src = base[..at].to_vec();
                src.extend_from_slice(run);
                src.extend_from_slice(&base[at..]);
                let want = reference(&src);
                for chunk in [4, 5, 13, 64] {
                    assert_eq!(copied(&src, true, chunk, 0).map(|_| ()), want.clone().map(|_| ()), "at {at} run {run:?} chunk {chunk}");
                }
            }
        }
    }

    // FMT-10: a valid 4-byte sequence then a stray continuation byte, ending
    // a chunk, is an error at the stray byte, not a carry.
    #[test]
    fn a_stray_continuation_byte_after_a_four_byte_sequence_is_reported_where_it_is() {
        let mut src = b"abcd".to_vec();
        src.extend_from_slice("\u{1F600}".as_bytes());
        src.push(0x80);
        src.extend_from_slice(b"wxyz");
        for chunk in 4..=12 {
            assert_eq!(copied(&src, true, chunk, 0), Err(8), "chunk {chunk}");
        }
    }

    // FMT-10
    #[test]
    fn a_sequence_cut_off_at_the_end_is_not_text() {
        for src in [&b"ok\xe2\x82"[..], b"\xf0\x9f\x98", b"abc\xc3"] {
            for chunk in [4, 64] {
                assert_eq!(copied(src, true, chunk, 0).map(|_| ()), reference(src).map(|_| ()));
            }
        }
    }

    // SLOT-36
    #[test]
    fn unchecked_bytes_are_copied_as_they_are_at_every_length_and_offset() {
        let src: Vec<u8> = (0..5100u32).map(|i| (i * 131 % 251) as u8).collect();
        for n in [0, 1, 15, 16, 17, 63, 64, 65, 200, 1000, 4999] {
            for off in 0..4 {
                let s = &src[off..off + n];
                assert_eq!(copied(s, false, 64, 0), Ok(s.to_vec()), "n {n} off {off}");
                assert_eq!(copied(s, false, 64, usize::MAX), Ok(s.to_vec()));
            }
        }
    }
}
