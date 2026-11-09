//! UTF-8 validation of `Str` bodies (BASE-5).

use std::sync::atomic::{AtomicU8, Ordering};

/// The offset of the first byte of `bytes` that is not part of valid
/// UTF-8, or `None` when all of it is valid.
pub fn first_invalid(bytes: &[u8]) -> Option<usize> {
    if simdutf8::basic::from_utf8(bytes).is_ok() {
        return None;
    }
    simdutf8::compat::from_utf8(bytes).err().map(|e| e.valid_up_to())
}

static SKIP: AtomicU8 = AtomicU8::new(0);

/// Whether the check is off: by the user (`MORLOC_SKIP_UTF8_CHECK`) or by
/// the program, built with `--unsafe-skip-utf8-check`, whose nexus sets
/// `MORLOC_PROGRAM_SKIPS_UTF8_CHECK`. Read once.
pub fn skip_utf8_check() -> bool {
    match SKIP.load(Ordering::Relaxed) {
        1 => false,
        2 => true,
        _ => {
            let skip = crate::null_check::env_flag("MORLOC_SKIP_UTF8_CHECK")
                || crate::null_check::env_flag("MORLOC_PROGRAM_SKIPS_UTF8_CHECK");
            SKIP.store(if skip { 2 } else { 1 }, Ordering::Relaxed);
            skip
        }
    }
}

/// `first_invalid`, unless the check is turned off.
pub fn checked_first_invalid(bytes: &[u8]) -> Option<usize> {
    if skip_utf8_check() {
        None
    } else {
        first_invalid(bytes)
    }
}

#[cfg(test)]
mod tests {
    use super::first_invalid;

    #[test]
    fn the_first_invalid_byte_is_found() {
        assert_eq!(first_invalid(b""), None);
        assert_eq!(first_invalid(b"plain ascii"), None);
        assert_eq!(first_invalid("caf\u{e9} \u{1F600}".as_bytes()), None);
        assert_eq!(first_invalid(b"ab\xffcd"), Some(2));
        assert_eq!(first_invalid(b"caf\xe9"), Some(3));
        assert_eq!(first_invalid(b"\xed\xa0\x80"), Some(0));
        assert_eq!(first_invalid(b"ok\xe2\x82"), Some(2));
        let long: Vec<u8> = std::iter::repeat(b'A').take(100_000).chain([0xc0, 0x80]).collect();
        assert_eq!(first_invalid(&long), Some(100_000));
    }
}
