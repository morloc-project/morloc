//! Manifest types -- thin re-export shim over the canonical schema
//! definitions in the `morloc-manifest` crate.
//!
//! This module exists only so that existing code in this crate (and
//! its consumers) can keep saying `crate::manifest::Manifest` without
//! caring whether the types live here or in a sibling crate. The
//! actual schema, with full doc comments and parsing logic, lives in
//! `data/rust/morloc-manifest/src/lib.rs`.

pub use morloc_manifest::{
    parse_manifest, read_manifest_payload, Arg, Command,
    GroupEntry, Manifest, Pool,
};
