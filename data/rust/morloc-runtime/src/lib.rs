pub mod error;
pub mod schema;
pub mod packet;
pub mod shm;
pub mod hash;
pub mod ipc;
pub mod json;
pub mod mpack;
// FFI and utility modules export #[no_mangle] extern "C" symbols.
// When the "no-ffi-exports" feature is active (nexus build), these modules
// are not compiled, preventing symbol conflicts with libmorloc.so.
// CSchema type is always available (used by nexus for Rust<->C conversion)
pub mod cschema;
pub mod ffi;
pub mod utility;
pub mod cache;
pub mod intrinsics;
pub mod voidstar;
pub mod json_ffi;
pub mod packet_ffi;
pub mod ipc_ffi;
pub mod http_ffi;
pub mod slurm_ffi;
pub mod manifest_ffi;
pub mod eval_ffi;
pub mod arrow_ffi;
pub mod pool_ffi;
pub mod daemon_ffi;
pub mod router_ffi;
pub mod cli;

/// Shared test SHM initialization. Call from all test modules.
#[cfg(test)]
pub(crate) fn init_test_shm() {
    use std::sync::Once;
    static INIT: Once = Once::new();
    INIT.call_once(|| {
        let tmpdir = std::env::temp_dir();
        let test_dir = tmpdir.join(format!("morloc_test_{}", std::process::id()));
        let _ = std::fs::create_dir_all(&test_dir);
        shm::shm_set_fallback_dir(test_dir.to_str().unwrap());
        let basename = format!("morloc_test_{}", std::process::id());
        shm::shinit(&basename, 0, 0x100000).unwrap(); // 1MB
    });
}

// Re-export core types at crate root
pub use error::MorlocError;
pub use schema::{Schema, SerialType};
pub use packet::{PacketHeader, PACKET_MAGIC};
pub use shm::{RelPtr, VolPtr, AbsPtr, Array, Tensor};
