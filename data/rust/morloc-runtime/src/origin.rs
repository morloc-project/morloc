//! Where a decoded value came from, which decides whether its fixed
//! lengths are checked and what a failed check means (DIM-3 in the
//! language spec): data from outside the program is always checked, and a
//! mismatch in it is bad input; a value the program built is checked only
//! in a debug build, and a mismatch in it is a defect (PANIC-14).

use morloc_runtime_types::error::MorlocError;

/// The environment variable a debug build's nexus sets for itself and its
/// pools.
pub const CODE_CHECKS_ENV: &str = "MORLOC_CODE_CHECKS";

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Origin {
    /// A command argument, a file, stdin.
    Outside,
    /// A value a pool, the nexus or the daemon of this program built.
    Program,
}

impl Origin {
    /// The origin a check runs for, or none when the check is off.
    pub fn checked(self) -> Option<Origin> {
        match self {
            Origin::Outside => Some(self),
            Origin::Program => crate::packet::code_checks().then_some(self),
        }
    }

    /// A failed fixed-length check on a value of this origin.
    pub fn length_failure(self, e: MorlocError) -> MorlocError {
        self.failure("has the wrong length", e)
    }

    /// An enum tag of a value of this origin that names no constructor.
    pub fn tag_failure(self, e: MorlocError) -> MorlocError {
        self.failure("holds an enum tag no constructor claims", e)
    }

    /// In a pool, a failed check of the program's own value ends the pool.
    fn failure(self, what: &str, e: MorlocError) -> MorlocError {
        match self {
            Origin::Outside => e,
            Origin::Program => {
                let msg = format!("internal error: a value the program built {what}: {e}");
                if crate::ipc_ffi::is_pool_process() {
                    morloc_runtime_types::panic::fatal(&msg);
                }
                MorlocError::Other(msg)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::schema::parse_schema;

    const CHILD_ENV: &str = "MORLOC_ORIGIN_TEST_CHILD";

    // `[1, 2]` as a flat voidstar blob and as msgpack.
    fn two_ints() -> (Vec<u8>, Vec<u8>) {
        let list = parse_schema("ai4").unwrap();
        let abs = crate::json::read_json_with_schema("[1, 2]", &list).unwrap();
        let mut flat = Vec::new();
        crate::voidstar::write_flat_to_writer(&mut flat, abs, &list).unwrap();
        (flat, crate::mpack::pack_with_schema(abs, &list).unwrap())
    }

    fn status_of_child(name: &str) -> i32 {
        let status = std::process::Command::new(std::env::current_exe().unwrap())
            .args([&format!("origin::tests::{name}"), "--exact", "--ignored", "--test-threads=1"])
            .env(CHILD_ENV, "1")
            .env(CODE_CHECKS_ENV, "1")
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .status()
            .unwrap();
        status.code().unwrap_or(-1)
    }

    #[test]
    fn only_outside_data_is_checked_in_a_release_build() {
        let _g = crate::init_test_shm();
        let three = parse_schema("a:3i4").unwrap();
        let (flat, mpk) = two_ints();
        assert!(crate::voidstar::read_binary_with_hint(&flat, &three, 0, Origin::Outside).is_err());
        assert!(crate::voidstar::read_binary_with_hint(&flat, &three, 0, Origin::Program).is_ok());
        assert!(crate::voidstar::check_in_place(&flat, &three, 0, Origin::Outside).is_err());
        assert!(crate::voidstar::check_in_place(&flat, &three, 0, Origin::Program).is_ok());
        assert!(crate::mpack::unpack_from(&mpk, &three, Origin::Outside).is_err());
        assert!(crate::mpack::unpack_from(&mpk, &three, Origin::Program).is_ok());
    }

    #[test]
    fn in_a_debug_build_a_wrong_length_in_a_pool_ends_the_pool() {
        assert_eq!(status_of_child("child_pool_reads_a_wrong_length"), morloc_runtime_types::panic::PANIC_EXIT_STATUS);
    }

    #[test]
    #[ignore]
    fn child_pool_reads_a_wrong_length() {
        if std::env::var_os(CHILD_ENV).is_none() {
            return;
        }
        crate::panic_ffi::install(None);
        let _g = crate::init_test_shm();
        let socket = std::ffi::CString::new("/nonexistent/origin-test.sock").unwrap();
        unsafe { crate::ipc_ffi::mlc_set_self_socket(socket.as_ptr()) };
        let (flat, _) = two_ints();
        let _ = crate::voidstar::read_binary_with_hint(&flat, &parse_schema("a:3i4").unwrap(), 0, Origin::Program);
        unsafe { libc::_exit(0) };
    }

    #[test]
    fn in_a_debug_build_a_wrong_length_outside_a_pool_is_an_internal_error() {
        assert_eq!(status_of_child("child_nexus_reads_a_wrong_length"), 0);
    }

    #[test]
    #[ignore]
    fn child_nexus_reads_a_wrong_length() {
        if std::env::var_os(CHILD_ENV).is_none() {
            return;
        }
        let _g = crate::init_test_shm();
        let (_, mpk) = two_ints();
        let e = crate::mpack::unpack_from(&mpk, &parse_schema("a:3i4").unwrap(), Origin::Program).unwrap_err();
        unsafe { libc::_exit(if e.to_string().starts_with("internal error") { 0 } else { 1 }) };
    }

    #[test]
    fn a_file_packet_the_program_wrote_is_marked_as_its_own() {
        let _g = crate::init_test_shm();
        let schema = std::ffi::CString::new("ai4").unwrap();
        let path = std::ffi::CString::new("/tmp/x.mpk").unwrap();
        unsafe {
            let cs = crate::ffi::parse_schema(schema.as_ptr(), &mut std::ptr::null_mut());
            for (packet, own) in [
                (crate::packet_ffi::make_program_file_packet(path.as_ptr(), cs, morloc_runtime_types::packet::PACKET_FORMAT_MSGPACK), true),
                (crate::packet_ffi::make_data_indirection_packet(path.as_ptr(), cs), false),
            ] {
                let offset = (*(packet as *const morloc_runtime_types::packet::PacketHeader)).offset as usize;
                let meta = std::slice::from_raw_parts(packet.add(32), offset);
                assert_eq!(morloc_runtime_types::packet::marks_program_file(meta), own);
                assert!(morloc_runtime_types::packet::read_schema_from_meta(std::slice::from_raw_parts(packet, 32 + offset)).unwrap().is_some());
                libc::free(packet as *mut libc::c_void);
            }
            crate::ffi::free_schema(cs);
        }
    }
}
