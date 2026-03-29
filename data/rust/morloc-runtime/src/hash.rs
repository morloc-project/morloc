//! xxHash64 wrapper using the twox-hash crate.
//! Replaces the 1500-line xxhash.h header.

use std::hash::Hasher;
use twox_hash::XxHash64;

const DEFAULT_SEED: u64 = 0;

/// Compute xxHash64 of a byte slice with the default seed.
pub fn xxh64(data: &[u8]) -> u64 {
    let mut hasher = XxHash64::with_seed(DEFAULT_SEED);
    hasher.write(data);
    hasher.finish()
}

/// Compute xxHash64 with a custom seed.
pub fn xxh64_with_seed(data: &[u8], seed: u64) -> u64 {
    let mut hasher = XxHash64::with_seed(seed);
    hasher.write(data);
    hasher.finish()
}

/// Mix multiple hash values together (for composite keys).
pub fn mix(a: u64, b: u64) -> u64 {
    // Use xxHash to mix two 64-bit values
    let mut hasher = XxHash64::with_seed(a);
    hasher.write(&b.to_le_bytes());
    hasher.finish()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_xxh64_empty() {
        let h = xxh64(b"");
        assert_ne!(h, 0); // xxHash of empty with seed 0 is a known non-zero value
    }

    #[test]
    fn test_xxh64_deterministic() {
        let a = xxh64(b"hello");
        let b = xxh64(b"hello");
        assert_eq!(a, b);
    }

    #[test]
    fn test_xxh64_different_inputs() {
        let a = xxh64(b"hello");
        let b = xxh64(b"world");
        assert_ne!(a, b);
    }

    #[test]
    fn test_mix_commutative_ish() {
        // mix is not commutative by design
        let ab = mix(1, 2);
        let ba = mix(2, 1);
        assert_ne!(ab, ba);
    }
}
