//! Stateless morloc runtime types and pure functions.
//!
//! Holds the surface of `morloc-runtime` that has no `static` or
//! `thread_local!` state: wire-format types, schema definitions, packet
//! headers, pure parsers, hash functions, and the C-ABI CSchema mirror.
//! Both `libmorloc.so` (cdylib) and `morloc-nexus` depend on this crate
//! as a normal rlib so they can share the canonical type definitions
//! without duplicating any per-process state.
//!
//! Anything that would touch a `static`, `thread_local!`, or
//! `Once`/`Mutex`/`AtomicXxx` singleton is intentionally NOT in this
//! crate -- it lives only in `morloc-runtime` so the nexus must reach
//! it through libmorloc.so's C ABI.

pub mod bearer;
pub mod utf8;
pub mod net_limits;
pub mod private_dir;
pub mod error;
pub mod hash;
pub mod http_query;
pub mod http_head;
pub mod call_body;
pub mod daemon_error;
pub mod shm_types;
pub mod shm_lock;
pub mod recoverable_lock;
pub mod process;
pub mod fork_generation;
pub mod publish_once;
pub mod fd;
pub mod panic;
pub mod spawn;
pub mod child_group;
pub mod owner_word;
pub mod schema;
pub mod dims;
pub mod cschema;
pub mod null_check;
pub mod packet;
pub mod pattern;
pub mod compression;
pub mod daemon_socket;
pub mod stream_handle;
pub mod stdio_proto;
pub mod wait_word;
pub mod width;
pub mod slice;
pub mod eval_status;
#[cfg(test)]
mod dispatch_guard;

pub use cschema::is_top_null;

/// The ABI / wire-format contract version, distinct from the release version.
/// The single source of truth is `#define MORLOC_ABI_VERSION` in
/// `data/rust/include/morloc.h`; this constant mirrors it (kept in lockstep by the
/// `abi_version_matches_header` test). `libmorloc.so` exposes it as the
/// `morloc_abi_version()` C function and `morloc-nexus` as `--abi-version`, so
/// provisioning can refuse a prebuilt binary whose version differs from the
/// compiler's expected value. Bump it (and the header) whenever the C ABI or
/// wire packet format changes.
pub const MORLOC_ABI_VERSION: u32 = 16;

#[cfg(test)]
mod abi_version_tests {
    use super::MORLOC_ABI_VERSION;

    // Keep the Rust mirror in lockstep with the canonical value in morloc.h.
    #[test]
    fn abi_version_matches_header() {
        let header = include_str!("../../include/morloc.h");
        let from_header = header
            .lines()
            .find_map(|l| l.trim().strip_prefix("#define MORLOC_ABI_VERSION"))
            .map(str::trim)
            .and_then(|s| s.parse::<u32>().ok())
            .expect("morloc.h must define MORLOC_ABI_VERSION as an integer");
        assert_eq!(
            from_header, MORLOC_ABI_VERSION,
            "MORLOC_ABI_VERSION in morloc.h ({from_header}) != Rust const ({MORLOC_ABI_VERSION}); bump both together"
        );
    }
}

/// FFI return codes for the print_voidstar / pretty_print_voidstar
/// C ABI: 0 ok, 1 error (see errmsg), 2 downstream pipe closed. Shared
/// so libmorloc.so and morloc-nexus can't drift.
pub const PRINT_RESULT_OK: i32 = 0;
pub const PRINT_RESULT_ERR: i32 = 1;

/// Shared i32 C-ABI result code (0=OK, 1=ERR, 2=PIPE_CLOSED) for the stdio
/// intrinsics that report a broken pipe by return code with no errmsg:
/// the print path AND mlc_write / mlc_flush / mlc_close. Named neutrally
/// (not PRINT_*) since @write/@flush/@close do not print; the C++ pool
/// mirrors it as MLC_RESULT_PIPE_CLOSED.
pub const MLC_RESULT_PIPE_CLOSED: i32 = 2;

/// SLOT-36: `mlc_write_begin` opened no batch; the binding writes the batch
/// with `mlc_write` instead.
pub const MLC_WRITE_FALLBACK: i32 = 3;

/// SLOT-37: the `mlc_write_begin` flag under which a placement that would
/// wait for queue room returns `MLC_WRITE_WOULD_BLOCK` instead.
pub const MLC_WRITE_NOWAIT: i32 = 1;

/// SLOT-37: nothing was placed; the binding calls `mlc_write_wait`, then
/// places again.
pub const MLC_WRITE_WOULD_BLOCK: i32 = 4;

/// SLOT-36: the `mlc_write_place_bytes` flag that says a `Str`'s bytes are
/// known UTF-8, so only a debug build checks them.
pub const MLC_PLACE_VALID_TEXT: u32 = 2;

/// SLOT-36: `mlc_write_place_bytes` found a `Str` that is not UTF-8 and
/// aborted the batch.
pub const MLC_WRITE_NOT_TEXT: i32 = 5;

/// The tag on an error that says this process lost shared memory (SHM-17,
/// PANIC-14).
pub const SHARED_MEMORY_LOST: &str = "[SHM-17] shared memory lost: ";

/// The exit status of a pool process that lost its shared memory: it ran out
/// of space (SHM-17) or could not map a volume it reads from (PANIC-14).
pub const EXIT_SHARED_MEMORY_LOST: i32 = 71;

/// LIFE-6: the id of the runtime source this crate was built from.
pub const BUILD_ID: u64 = include!(concat!(env!("OUT_DIR"), "/build_id.rs"));

/// LIFE-6: the variable through which a nexus tells its pools the build id of
/// the libmorloc it loaded, in hex.
pub const BUILD_ID_ENV: &str = "MORLOC_BUILD_ID";

/// LIFE-6: the exit status of a process that refused to run against another
/// build of morloc.
pub const EXIT_BUILD_SKEW: i32 = 75;

pub const BUILD_SKEW_RESTART: &str = "morloc was upgraded while this program was starting; run it again";

/// LIFE-6: whether a process that loaded the libmorloc `loaded` must not
/// run, given the id of the installed binding it uses (`binding`, none for
/// code compiled into the program) and the nexus's id (`nexus`, from
/// `BUILD_ID_ENV`, if set).
pub fn build_skew(loaded: u64, binding: Option<u64>, nexus: Option<&str>) -> Option<&'static str> {
    let nexus_differs = nexus.is_some_and(|n| u64::from_str_radix(n, 16).ok() != Some(loaded));
    let binding_differs = binding.is_some_and(|b| b != loaded);
    (nexus_differs || binding_differs).then_some(BUILD_SKEW_RESTART)
}

#[cfg(test)]
#[path = "../build_id.rs"]
mod build_id_source;

#[cfg(test)]
mod build_skew_tests {
    use super::*;

    #[test]
    fn a_process_runs_only_on_the_build_it_was_made_with() {
        let hex = |v: u64| format!("{v:016x}");
        assert_eq!(build_skew(7, Some(7), None), None);
        assert_eq!(build_skew(7, None, None), None);
        assert_eq!(build_skew(7, Some(7), Some(&hex(7))), None);
        assert_eq!(build_skew(7, None, Some(&hex(7))), None);
        assert_eq!(build_skew(7, None, Some(&hex(8))), Some(BUILD_SKEW_RESTART));
        assert_eq!(build_skew(7, Some(7), Some("not hex")), Some(BUILD_SKEW_RESTART));
        assert_eq!(build_skew(7, Some(8), None), Some(BUILD_SKEW_RESTART));
        assert_eq!(build_skew(7, Some(8), Some(&hex(7))), Some(BUILD_SKEW_RESTART));
    }

    use super::build_id_source as source;

    fn copy_tree(from: &std::path::Path, to: &std::path::Path) {
        std::fs::create_dir_all(to).unwrap();
        for e in std::fs::read_dir(from).unwrap().flatten() {
            let (p, q) = (e.path(), to.join(e.file_name()));
            if p.is_dir() {
                if e.file_name() != "target" {
                    copy_tree(&p, &q);
                }
            } else {
                std::fs::copy(&p, &q).unwrap();
            }
        }
    }

    #[test]
    fn the_build_id_is_the_hash_of_the_runtime_source_wherever_it_lies() {
        let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
        assert_eq!(source::source_id(root), BUILD_ID);
        let copy = std::env::temp_dir().join(format!("morloc-build-id-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&copy);
        for c in source::SOURCE_CRATES {
            copy_tree(&root.join(c), &copy.join(c));
        }
        for f in ["Cargo.toml", "Cargo.lock"] {
            std::fs::copy(root.join(f), copy.join(f)).unwrap();
        }
        copy_tree(&root.join("include"), &copy.join("include"));
        assert_eq!(source::source_id(&copy), BUILD_ID, "a copy of the source has the same id");
        let header = copy.join("include").join("morloc.h");
        let original = std::fs::read(&header).unwrap();
        let mut edited = original.clone();
        edited.push(b'\n');
        std::fs::write(&header, &edited).unwrap();
        assert_ne!(source::source_id(&copy), BUILD_ID, "another header has another id");
        std::fs::write(&header, &original).unwrap();
        let lib = copy.join("morloc-runtime").join("src").join("lib.rs");
        let mut text = std::fs::read(&lib).unwrap();
        text.push(b'\n');
        std::fs::write(&lib, &text).unwrap();
        assert_ne!(source::source_id(&copy), BUILD_ID, "another source has another id");
        std::fs::write(&lib, &text[..text.len() - 1]).unwrap();
        std::fs::write(lib.with_file_name(".lib.rs.swp"), b"editor").unwrap();
        assert_eq!(source::source_id(&copy), BUILD_ID, "an editor file is not source");
        let _ = std::fs::remove_dir_all(&copy);
    }
}

/// Back-compat alias for the print path.
pub const PRINT_RESULT_PIPE_CLOSED: i32 = MLC_RESULT_PIPE_CLOSED;

/// Codes of `mlc_write_raw_subpacket`: the print codes, and one for a
/// sub-packet it leaves to the general path.
pub const RAW_WRITTEN: i32 = PRINT_RESULT_OK;
pub const RAW_FAILED: i32 = PRINT_RESULT_ERR;
pub const RAW_PIPE_CLOSED: i32 = MLC_RESULT_PIPE_CLOSED;
pub const RAW_NOT_HANDLED: i32 = 3;

/// Sentinel returned by `normalize_data_packet_to_fd` (an i64 byte count,
/// negative on error) when the destination fd's downstream closed the pipe
/// (EPIPE). Distinct from the generic `-1` error so the nexus stdout
/// bridge can classify it as a broken pipe rather than a generic failure.
pub const PACKET_TO_FD_PIPE_CLOSED: i64 = -2;
