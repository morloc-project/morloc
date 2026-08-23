//! Dependency-resolution kernel: environment-spec parsing, constraint
//! intersection, the language-support table, and pixi manifest rendering.
//!
//! Shared by `morloc-manager` (the outside orchestrator) and `morloc-env` (the
//! in-env agent). The kernel is agnostic to how an environment is located; the
//! caller supplies concrete paths. Its error type is `DepsError`; a consumer
//! converts it into its own error at the crate boundary.

pub mod abi;
pub mod constraint;
// envspec/langsupport deserialize the compiler's JSON contracts in full; not
// every schema field is consumed yet (e.g. module pins, cpp std, default
// versions), so unused-field warnings on those mirror types are expected.
#[allow(dead_code)]
pub mod envspec;
pub mod envstore;
pub mod error;
#[allow(dead_code)]
pub mod langsupport;
pub mod layout;
pub mod pixi;
pub mod platform;
