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
pub mod shm_companion;
pub mod hash;
// Re-export the daemon_socket and shm_types modules from the types
// crate at the same path so existing C-ABI signatures referencing
// `crate::shm` constants keep working; daemon_socket gives daemon_ffi
// the `MorlocSocket` struct without re-defining it.
pub use morloc_runtime_types::shm_types;
pub use morloc_runtime_types::daemon_socket;
pub use morloc_runtime_types::compression;
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
mod c_abi_layout;
pub mod eval_arena;
pub mod eval_ffi;
pub mod stream;
pub mod handle_scan;
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
pub mod debug;

/// Serializes tests against the process-global SHM arena. There is one arena
/// per process, so a test that tears it down cannot run beside a test that is
/// allocating in it: readers share the arena built by `init_test_shm`, while
/// a test that drives `shinit`/`shclose` itself takes the write guard.
#[cfg(test)]
static SHM_TEST_ARENA: std::sync::RwLock<()> = std::sync::RwLock::new(());

/// Shared test SHM initialization. Call from all test modules and hold the
/// returned guard for the body of the test.
#[cfg(test)]
#[must_use]
pub(crate) fn init_test_shm() -> std::sync::RwLockReadGuard<'static, ()> {
    let guard = SHM_TEST_ARENA
        .read()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
    ensure_test_arena();
    guard
}

/// Exclusive access for tests that build and tear down their own arena.
#[cfg(test)]
#[must_use]
pub(crate) fn own_test_shm() -> std::sync::RwLockWriteGuard<'static, ()> {
    SHM_TEST_ARENA
        .write()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
}

/// Exclusive access with the shared arena guaranteed live. For tests that
/// drive process-global companion state -- the stream and stdio registries --
/// which, like the arena, exist once per process and cannot be shared.
#[cfg(test)]
#[must_use]
pub(crate) fn own_test_registry() -> std::sync::RwLockWriteGuard<'static, ()> {
    let guard = own_test_shm();
    ensure_test_arena();
    guard
}

#[cfg(test)]
fn ensure_test_arena() {
    static INIT: std::sync::Mutex<()> = std::sync::Mutex::new(());
    let _init = INIT.lock().unwrap_or_else(|poisoned| poisoned.into_inner());
    // Deliberately not one-shot: an arena-owning test may have called shclose
    // since the last caller, resetting the allocator to its pre-shinit state.
    if shm::get_common_basename().is_empty() {
        let tmpdir = std::env::temp_dir();
        let test_dir = tmpdir.join(format!("morloc_test_{}", std::process::id()));
        let _ = std::fs::create_dir_all(&test_dir);
        shm::shm_set_fallback_dir(test_dir.to_str().unwrap());
        let basename = format!("morloc-{}-test-arena", std::process::id());
        shm::shinit(&basename, 0, 0x100000).unwrap(); // 1MB
    }
}

// Re-export core types at crate root
pub use error::MorlocError;
pub use schema::{Schema, SerialType};
pub use packet::{PacketHeader, PACKET_MAGIC};
pub use shm::{RelPtr, VolPtr, AbsPtr, Array};
