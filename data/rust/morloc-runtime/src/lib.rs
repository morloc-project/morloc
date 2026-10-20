#[cfg(not(panic = "unwind"))]
compile_error!("morloc needs panic = \"unwind\" (model/runtime/panic.md PANIC-8)");
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
pub mod compare;
#[cfg(test)]
pub mod deep_tests;
#[cfg(test)]
mod arena_ref_tests;
#[cfg(test)]
mod alloc_bench;
#[cfg(test)]
mod stream_bench;
#[cfg(test)]
mod layout_bench;
#[cfg(test)]
mod layout_props;
#[cfg(test)]
mod pins;
pub mod packet;
pub mod shm;
pub mod shm_companion;
pub mod shm_stats;
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
mod stream_walk;
mod stream_format;
mod write_behind;
pub mod custody;
pub mod handle_scan;
pub mod arrow_shm;
pub mod lease;
pub mod arrow_ffi;
pub mod arrow_ipc_reader;
pub mod delimited;
pub mod pool_ffi;
pub mod fork_pool;
pub mod crash;
pub mod daemon_ffi;
pub mod panic_ffi;
pub mod router_ffi;
pub mod null_check;
pub mod cli;
pub mod log;
pub mod run;
pub mod lifeline;
mod fork_policy;
pub mod debug;

/// Serializes tests against the process-global SHM arena. There is one arena
/// per process, so a test that tears it down cannot run beside a test that is
/// allocating in it: readers share the arena built by `init_test_shm`, while
/// a test that drives `shinit`/`shclose` itself takes the exclusive guard.
///
/// How readers and writers wait for each other is set by
/// `test_arena_lock_tests`.
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
    writers_waiting: usize,
}

#[cfg(test)]
const READER_PATIENCE: std::time::Duration = std::time::Duration::from_millis(500);

#[cfg(test)]
thread_local! {
    static READS_HELD: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

#[cfg(test)]
static SHM_TEST_ARENA: TestArenaLock = TestArenaLock {
    state: std::sync::Mutex::new(TestArenaState { readers: 0, writer: false, writers_waiting: 0 }),
    turn: std::sync::Condvar::new(),
};

#[cfg(test)]
impl TestArenaLock {
    fn state(&self) -> std::sync::MutexGuard<'_, TestArenaState> {
        self.state.lock().unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    fn read(&self) {
        let nested = READS_HELD.with(|n| n.get()) > 0;
        let deadline = std::time::Instant::now() + READER_PATIENCE;
        let mut st = self.state();
        loop {
            let now = std::time::Instant::now();
            let defer = !nested && st.writers_waiting > 0 && now < deadline;
            if !st.writer && !defer {
                break;
            }
            st = if st.writer {
                self.turn.wait(st).unwrap_or_else(|poisoned| poisoned.into_inner())
            } else {
                self.turn.wait_timeout(st, deadline - now).unwrap_or_else(|poisoned| poisoned.into_inner()).0
            };
        }
        st.readers += 1;
        drop(st);
        READS_HELD.with(|n| n.set(n.get() + 1));
    }

    fn write(&self) {
        let mut st = self.state();
        st.writers_waiting += 1;
        while st.writer || st.readers > 0 {
            st = self.turn.wait(st).unwrap_or_else(|poisoned| poisoned.into_inner());
        }
        st.writers_waiting -= 1;
        st.writer = true;
    }
}

/// Shared hold on the test arena; see `TestArenaLock`.
#[cfg(test)]
pub(crate) struct ArenaShared(std::thread::ThreadId);

/// Exclusive hold on the test arena; see `TestArenaLock`.
#[cfg(test)]
pub(crate) struct ArenaOwned(());

#[cfg(test)]
impl Drop for ArenaShared {
    fn drop(&mut self) {
        if self.0 == std::thread::current().id() {
            READS_HELD.with(|n| n.set(n.get().saturating_sub(1)));
        }
        SHM_TEST_ARENA.state().readers -= 1;
        SHM_TEST_ARENA.turn.notify_all();
    }
}

#[cfg(test)]
impl Drop for ArenaOwned {
    fn drop(&mut self) {
        // SLOT-32: a writer left running takes a share of every later test's
        // queue bytes.
        if !std::thread::panicking() {
            let left = stray_stream_writers();
            if !left.is_empty() {
                SHM_TEST_ARENA.state().writer = false;
                SHM_TEST_ARENA.turn.notify_all();
                panic!("a test left streams written: {left:?}");
            }
        }
        SHM_TEST_ARENA.state().writer = false;
        SHM_TEST_ARENA.turn.notify_all();
    }
}

#[cfg(test)]
mod source_rules;

#[cfg(test)]
mod test_arena_lock_tests {
    // A reader that joins a thread taking its own read guard must finish
    // even with a writer queued in between. The bound runs from the outer
    // read: taking it may wait on a writer elsewhere in the run.
    #[test]
    fn a_nested_reader_is_not_blocked_by_a_queued_writer() {
        let (held_tx, held_rx) = std::sync::mpsc::channel();
        let (done_tx, done_rx) = std::sync::mpsc::channel();
        std::thread::spawn(move || {
            let outer = crate::init_test_shm();
            held_tx.send(()).unwrap();
            let (queued_tx, queued_rx) = std::sync::mpsc::channel();
            let writer = std::thread::spawn(move || {
                queued_tx.send(()).unwrap();
                drop(crate::own_test_shm());
            });
            queued_rx.recv().unwrap();
            std::thread::sleep(std::time::Duration::from_millis(100));
            std::thread::spawn(|| drop(crate::init_test_shm())).join().unwrap();
            done_tx.send(()).unwrap();
            drop(outer);
            writer.join().unwrap();
        });
        held_rx.recv().unwrap();
        done_rx
            .recv_timeout(std::time::Duration::from_secs(10))
            .expect("nested readers deadlocked behind a queued writer");
    }

    // A reader whose thread already holds a read guard takes another at once,
    // ahead of a queued writer.
    #[test]
    fn a_reader_nested_on_its_own_thread_does_not_wait_for_a_queued_writer() {
        let outer = crate::init_test_shm();
        let (queued_tx, queued_rx) = std::sync::mpsc::channel();
        let writer = std::thread::spawn(move || {
            queued_tx.send(()).unwrap();
            drop(crate::own_test_shm());
        });
        queued_rx.recv().unwrap();
        std::thread::sleep(std::time::Duration::from_millis(100));
        let began = std::time::Instant::now();
        drop(crate::init_test_shm());
        let took = began.elapsed();
        drop(outer);
        writer.join().unwrap();
        assert!(took < super::READER_PATIENCE / 2, "a nested read on its own thread waited {took:?}");
    }
}

/// The paths of the streams this process still writes, waiting a moment for
/// writers that are finishing; any left are stopped.
#[cfg(test)]
fn stray_stream_writers() -> Vec<String> {
    if shm::get_common_basename().is_empty() {
        return Vec::new();
    }
    let began = std::time::Instant::now();
    let mut left = custody::hosted_slots();
    while !left.is_empty() && began.elapsed() < std::time::Duration::from_secs(2) {
        std::thread::sleep(std::time::Duration::from_millis(10));
        left = custody::hosted_slots();
    }
    let paths: Vec<String> = left
        .iter()
        .map(|&(slot, gen)| stream::handle_path(stream::pack_handle(gen, slot)).unwrap_or_else(|e| e.to_string()))
        .collect();
    if !paths.is_empty() {
        custody::stop_before_unmap();
    }
    paths
}

/// Shared test SHM initialization. Call from all test modules and hold the
/// returned guard for the body of the test.
#[cfg(test)]
#[must_use]
pub(crate) fn init_test_shm() -> ArenaShared {
    SHM_TEST_ARENA.read();
    let guard = ArenaShared(std::thread::current().id());
    ensure_test_arena();
    guard
}

#[cfg(test)]
pub(crate) fn signal_test_child(pid: libc::pid_t, sig: libc::c_int) {
    // A pid of 0 or -1 names a process group or every process, which on a CI
    // runner includes the runner itself.
    assert!(pid > 0 && pid != unsafe { libc::getpid() }, "refusing to signal pid {pid}");
    unsafe { libc::kill(pid, sig) };
}

/// Exclusive access for tests that build and tear down their own arena.
#[cfg(test)]
#[must_use]
pub(crate) fn own_test_shm() -> ArenaOwned {
    SHM_TEST_ARENA.write();
    let left = stray_stream_writers();
    if !left.is_empty() {
        eprintln!("streams written by an earlier test were still open: {left:?}");
    }
    ArenaOwned(())
}

/// Write an executable script from another process. A file this process
/// opened for writing can be inherited by a test's forked child, and exec'ing
/// it fails with ETXTBSY until that child exits.
#[cfg(test)]
pub(crate) fn write_test_executable(path: &std::path::Path, script: &str) {
    let status = std::process::Command::new("sh")
        .args(["-c", "printf '%s' \"$1\" > \"$2\" && chmod 755 \"$2\"", "sh", script])
        .arg(path)
        .status()
        .unwrap();
    assert!(status.success(), "writing {} failed", path.display());
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
    static SWEPT: std::sync::Once = std::sync::Once::new();
    SWEPT.call_once(sweep_dead_test_arenas);
    if shm::get_common_basename().is_empty() {
        let tmpdir = std::env::temp_dir();
        let test_dir = tmpdir.join(format!("morloc_test_{}", std::process::id()));
        let _ = std::fs::create_dir_all(&test_dir);
        shm::shm_set_fallback_dir(test_dir.to_str().unwrap());
        let basename = format!("/morloc-{}-test-arena", std::process::id());
        shm::shinit(&basename, 0x100000).unwrap(); // 1MB
        static AT_EXIT: std::sync::Once = std::sync::Once::new();
        AT_EXIT.call_once(|| unsafe {
            libc::atexit(remove_test_arena);
        });
    }
    crate::custody::test_host();
}

/// Remove this process's test arena at exit: statics are never dropped, so
/// without this every test run leaves its arena behind. Only names are
/// removed, taking no lock a stuck test thread might hold; the kernel
/// releases the mappings. Volumes are found by their markers, since growth
/// volumes take random indices.
#[cfg(test)]
extern "C" fn remove_test_arena() {
    remove_marked_dir(&std::env::temp_dir().join(format!("morloc_test_{}", std::process::id())));
}

/// Remove what earlier test processes that were killed (a timeout, an
/// interrupted run) left behind: they never reach their exit cleanup, and
/// their shared memory would otherwise stay until reboot, filling /dev/shm
/// run after run. Their directories end in the dead process's pid.
#[cfg(test)]
fn sweep_dead_test_arenas() {
    let Ok(entries) = std::fs::read_dir(std::env::temp_dir()) else { return };
    for e in entries.flatten() {
        let name = e.file_name().to_string_lossy().into_owned();
        if !(name.starts_with("morloc_test_") || name.starts_with("morloc_layout_props_")) {
            continue;
        }
        let Some(pid) = name.rsplit('_').next().and_then(|p| p.parse::<u32>().ok()) else { continue };
        if !morloc_runtime_types::process::alive(pid, 0) {
            remove_marked_dir(&e.path());
        }
    }
}

/// The shared-memory objects recorded by markers in `dir` (see
/// `shm::marker_path`), by name with its leading `/`.
#[cfg(test)]
pub(crate) fn marked_segments(dir: &std::path::Path) -> Vec<String> {
    std::fs::read_dir(dir)
        .map(|d| {
            d.flatten()
                .filter_map(|e| e.file_name().into_string().ok())
                .filter_map(|n| n.strip_suffix(shm::MARKER_SUFFIX).map(|s| format!("/{s}")))
                .collect()
        })
        .unwrap_or_default()
}

/// A fallback directory of a test's own, set for the process while the
/// value lives. Dropping it restores the previous one and removes the
/// directory with every shared-memory object it records, so no later test
/// is left pointing at a directory that is gone.
#[cfg(test)]
pub(crate) struct ScopedFallback {
    dir: std::path::PathBuf,
    prev: Option<String>,
}

#[cfg(test)]
impl ScopedFallback {
    pub(crate) fn new(tag: &str) -> Self {
        let dir = std::env::temp_dir().join(format!("morloc_test_{tag}_{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let prev = shm::get_fallback_dir();
        shm::shm_set_fallback_dir(dir.to_str().unwrap());
        ScopedFallback { dir, prev }
    }

    pub(crate) fn path(&self) -> &std::path::Path {
        &self.dir
    }
}

#[cfg(test)]
impl Drop for ScopedFallback {
    fn drop(&mut self) {
        shm::shm_set_fallback_dir(self.prev.as_deref().unwrap_or(""));
        remove_marked_dir(&self.dir);
    }
}

/// Remove every shared-memory object recorded in `dir`, then `dir`.
#[cfg(test)]
pub(crate) fn remove_marked_dir(dir: &std::path::Path) {
    for name in marked_segments(dir) {
        if let Ok(c) = std::ffi::CString::new(name) {
            unsafe { libc::shm_unlink(c.as_ptr()) };
        }
    }
    let _ = std::fs::remove_dir_all(dir);
}

// Re-export core types at crate root
pub use error::MorlocError;
pub use schema::{Schema, SerialType};
pub use packet::{PacketHeader, PACKET_MAGIC};
pub use shm::{RelPtr, VolPtr, AbsPtr, Array};
