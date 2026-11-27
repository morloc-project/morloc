//! Re-export of the shared xxh64 hash functions. Definitions live in
//! `morloc-runtime-types::hash` so the nexus and libmorloc.so share the
//! canonical implementation.

pub use morloc_runtime_types::hash::*;
