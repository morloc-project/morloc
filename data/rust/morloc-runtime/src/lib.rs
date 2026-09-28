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
pub mod walk;
#[cfg(test)]
pub mod deep_tests;
#[cfg(test)]
mod layout_bench;
#[cfg(test)]
mod layout_props;
#[cfg(test)]
mod pins;
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
pub mod cell;
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
mod c_abi_prototypes;
pub mod eval_arena;
pub mod eval_ffi;
pub mod stream;
pub mod handle_scan;
pub mod arrow_shm;
pub mod arrow_ffi;
pub mod arrow_ipc_reader;
pub mod pool_ffi;
pub mod crash;
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
/// a test that drives `shinit`/`shclose` itself takes the exclusive guard.
///
/// Readers are preferred: a reader waits only while a writer holds the lock,
/// never for one that is queued. A test holding a read guard may therefore
/// run work on another thread that takes its own read guard and join it;
/// with a writer-preferring lock (std's `RwLock` on Linux) a writer queued
/// between the two reads deadlocks all three.
#[cfg(test)]
pub(crate) struct TestArenaLock {
    state: std::sync::Mutex<TestArenaState>,
    turn: std::sync::Condvar,
}

#[cfg(test)]
#[derive(Default)]
struct TestArenaState {
    readers: usize,
    writer: bool,
}

#[cfg(test)]
static SHM_TEST_ARENA: TestArenaLock = TestArenaLock {
    state: std::sync::Mutex::new(TestArenaState { readers: 0, writer: false }),
    turn: std::sync::Condvar::new(),
};

#[cfg(test)]
impl TestArenaLock {
    fn state(&self) -> std::sync::MutexGuard<'_, TestArenaState> {
        self.state.lock().unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    fn wait_until(&self, ready: impl Fn(&TestArenaState) -> bool) -> std::sync::MutexGuard<'_, TestArenaState> {
        let mut st = self.state();
        while !ready(&st) {
            st = self.turn.wait(st).unwrap_or_else(|poisoned| poisoned.into_inner());
        }
        st
    }
}

/// Shared hold on the test arena; see `TestArenaLock`.
#[cfg(test)]
pub(crate) struct ArenaShared(());

/// Exclusive hold on the test arena; see `TestArenaLock`.
#[cfg(test)]
pub(crate) struct ArenaOwned(());

#[cfg(test)]
impl Drop for ArenaShared {
    fn drop(&mut self) {
        SHM_TEST_ARENA.state().readers -= 1;
        SHM_TEST_ARENA.turn.notify_all();
    }
}

#[cfg(test)]
impl Drop for ArenaOwned {
    fn drop(&mut self) {
        SHM_TEST_ARENA.state().writer = false;
        SHM_TEST_ARENA.turn.notify_all();
    }
}

#[cfg(test)]
mod test_arena_lock_tests {
    // A reader that joins a thread taking its own read guard must finish
    // even with a writer queued in between.
    #[test]
    fn a_nested_reader_is_not_blocked_by_a_queued_writer() {
        let (done_tx, done_rx) = std::sync::mpsc::channel();
        std::thread::spawn(move || {
            let outer = crate::init_test_shm();
            let (queued_tx, queued_rx) = std::sync::mpsc::channel();
            let writer = std::thread::spawn(move || {
                queued_tx.send(()).unwrap();
                drop(crate::own_test_shm());
            });
            queued_rx.recv().unwrap();
            std::thread::sleep(std::time::Duration::from_millis(100));
            std::thread::spawn(|| drop(crate::init_test_shm())).join().unwrap();
            drop(outer);
            writer.join().unwrap();
            done_tx.send(()).unwrap();
        });
        done_rx
            .recv_timeout(std::time::Duration::from_secs(10))
            .expect("nested readers deadlocked behind a queued writer");
    }
}

/// Shared test SHM initialization. Call from all test modules and hold the
/// returned guard for the body of the test.
#[cfg(test)]
#[must_use]
pub(crate) fn init_test_shm() -> ArenaShared {
    SHM_TEST_ARENA.wait_until(|st| !st.writer).readers += 1;
    let guard = ArenaShared(());
    ensure_test_arena();
    guard
}

/// Exclusive access for tests that build and tear down their own arena.
#[cfg(test)]
#[must_use]
pub(crate) fn own_test_shm() -> ArenaOwned {
    SHM_TEST_ARENA.wait_until(|st| !st.writer && st.readers == 0).writer = true;
    ArenaOwned(())
}

/// Exclusive access with the shared arena guaranteed live. For tests that
/// drive process-global companion state -- the stream and stdio registries --
/// which, like the arena, exist once per process and cannot be shared.
#[cfg(test)]
#[must_use]
pub(crate) fn own_test_registry() -> ArenaOwned {
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
        let basename = format!("/morloc-{}-test-arena", std::process::id());
        shm::shinit(&basename, shm::PRIMARY_VOLUME, 0x100000).unwrap(); // 1MB
        static AT_EXIT: std::sync::Once = std::sync::Once::new();
        AT_EXIT.call_once(|| unsafe {
            libc::atexit(remove_test_arena);
        });
    }
}

/// Remove this process's test arena at exit: statics are never dropped, so
/// without this every test run leaves its arena in /dev/shm. Only names are
/// removed, taking no lock a stuck test thread might hold; the kernel
/// releases the mappings. Volumes are found by name, since growth volumes
/// take random indices.
#[cfg(test)]
extern "C" fn remove_test_arena() {
    let prefix = format!("morloc-{}-test-arena", std::process::id());
    if let Ok(entries) = std::fs::read_dir("/dev/shm") {
        for e in entries.flatten() {
            let name = e.file_name();
            if name.to_string_lossy().starts_with(&prefix) {
                if let Ok(c) = std::ffi::CString::new(format!("/{}", name.to_string_lossy())) {
                    unsafe { libc::shm_unlink(c.as_ptr()) };
                }
            }
        }
    }
    let _ = std::fs::remove_dir_all(std::env::temp_dir().join(format!("morloc_test_{}", std::process::id())));
}

// Re-export core types at crate root
pub use error::MorlocError;
pub use schema::{Schema, SerialType};
pub use packet::{PacketHeader, PACKET_MAGIC};
pub use shm::{RelPtr, VolPtr, AbsPtr, Array};
