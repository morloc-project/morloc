// Modules that come entirely (error, hash, schema, cschema) or
// partially (packet, null_check) from morloc-runtime-types live as
// thin re-export shims in this crate so existing `crate::error::*`,
// `crate::schema::*`, etc. call sites inside libmorloc.so continue to
// compile unchanged. The canonical type definitions live in the types
// crate so nexus and libmorloc.so share them via the rlib without
// duplicating any state.
pub mod error;
pub mod schema;
pub mod recur;
pub mod packet;
pub mod shm;
pub mod hash;
// Re-export the daemon_socket and shm_types modules from the types
// crate at the same path so existing C-ABI signatures referencing
// `crate::shm` constants keep working; daemon_socket gives daemon_ffi
// the `MorlocSocket` struct without re-defining it.
pub use morloc_runtime_types::shm_types;
pub use morloc_runtime_types::daemon_socket;
pub mod ipc;
pub mod json;
pub mod mpack;
// FFI modules export #[no_mangle] extern "C" symbols that constitute
// libmorloc.so's public surface. The nexus reaches these via DT_NEEDED;
// it does not link this crate as an rlib (see Cargo.toml's crate-type
// comment for why).
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
pub mod slurm_bridge;
pub mod manifest_ffi;
pub mod eval_arena;
pub mod eval_ffi;
pub mod arrow_ffi;
pub mod arrow_ipc_reader;
pub mod pool_ffi;
pub mod daemon_ffi;
pub mod router_ffi;
pub mod null_check;
pub mod cli;
pub mod config_ffi;
pub mod log;
pub mod run;

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
pub use shm::{RelPtr, VolPtr, AbsPtr, Array};
