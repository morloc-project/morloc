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

/// The output the stage of this run saved, held open for the life of the
/// process so that its inode is never reused, and that inode. Only a replay
/// child's nexus sets it (FMT-10).
struct Stage {
    _file: std::fs::File,
    identity: (u64, u64),
}

static STAGE: morloc_runtime_types::publish_once::PublishOnce<Stage> =
    morloc_runtime_types::publish_once::PublishOnce::new();

/// The directory and file name of `path` when it is where the stage of a
/// multi-output run saves its output: `stream.pkt` or `value.pkt` in a
/// `morloc-stage-XXXXXX` directory of the temporary directory the nexus
/// uses (orchestrate.rs `make_stage_dir`).
fn stage_output_at(path: &str) -> Option<(&str, &str)> {
    let base = std::env::var("MORLOC_TMPDIR")
        .ok()
        .filter(|d| !d.is_empty())
        .unwrap_or_else(|| std::env::temp_dir().to_string_lossy().into_owned());
    let rest = path.strip_prefix(base.trim_end_matches('/'))?.strip_prefix("/morloc-stage-")?;
    let (suffix, name) = rest.split_once('/')?;
    let unique = suffix.len() == 6 && suffix.bytes().all(|b| b.is_ascii_alphanumeric());
    (unique && (name == "stream.pkt" || name == "value.pkt")).then(|| (&path[..path.len() - name.len() - 1], name))
}

/// Take `path`, the last input of the hidden replay command, as the output
/// the stage of this run saved, which the replay reads as the program's
/// own (FMT-10). Trust comes from where the file is, never from what it
/// holds: it must be named as a stage names it, in a directory owned by
/// this user that no one else may enter, as a regular file with one link,
/// owned by this user and writable by no one else. The file is opened
/// without following a link and kept open; anything else stays data from
/// outside.
pub fn trust_stage_file(path: &str) -> bool {
    match stage_file_at(path) {
        Ok(stage) => {
            let identity = stage.identity;
            STAGE.get_or_init(|| stage).identity == identity
        }
        Err(why) => {
            crate::morloc_trace!("[origin] {} is read as data from outside: {}", path, why);
            false
        }
    }
}

fn stage_file_at(path: &str) -> Result<Stage, &'static str> {
    use std::os::fd::AsRawFd;
    use std::os::unix::io::FromRawFd;
    let (dir, name) = stage_output_at(path).ok_or("it is not where a stage saves its output")?;
    let (Ok(dir), Ok(name)) = (std::ffi::CString::new(dir), std::ffi::CString::new(name)) else {
        return Err("its path holds a NUL byte");
    };
    let euid = unsafe { libc::geteuid() };
    // SAFETY: each descriptor is owned by the value made from it at once.
    let dir = unsafe {
        let fd = libc::open(dir.as_ptr(), libc::O_RDONLY | libc::O_DIRECTORY | libc::O_NOFOLLOW | libc::O_CLOEXEC);
        if fd < 0 {
            return Err("its directory cannot be opened without following a link");
        }
        std::os::fd::OwnedFd::from_raw_fd(fd)
    };
    let d = stat_of(dir.as_raw_fd()).ok_or("its directory cannot be read")?;
    if d.st_mode & libc::S_IFMT != libc::S_IFDIR || d.st_uid != euid || d.st_mode & 0o077 != 0 {
        return Err("its directory is not this user's alone");
    }
    let file = unsafe {
        let fd = libc::openat(dir.as_raw_fd(), name.as_ptr(), libc::O_RDONLY | libc::O_NOFOLLOW | libc::O_CLOEXEC);
        if fd < 0 {
            return Err("it cannot be opened without following a link");
        }
        std::fs::File::from_raw_fd(fd)
    };
    let st = stat_of(file.as_raw_fd()).ok_or("it cannot be read")?;
    if st.st_mode & libc::S_IFMT != libc::S_IFREG || st.st_uid != euid || st.st_nlink != 1 || st.st_mode & 0o022 != 0 {
        return Err("it is not a regular file with one link, this user's, writable by no one else");
    }
    Ok(Stage { _file: file, identity: (st.st_dev as u64, st.st_ino as u64) })
}

fn stat_of(fd: libc::c_int) -> Option<libc::stat> {
    let mut st: libc::stat = unsafe { std::mem::zeroed() };
    (unsafe { libc::fstat(fd, &mut st) } == 0).then_some(st)
}

/// The origin of the data in the file with this device and inode.
pub fn of_file(identity: (u64, u64)) -> Origin {
    if STAGE.get().is_some_and(|s| s.identity == identity) { Origin::Program } else { Origin::Outside }
}

/// The origin of the data in the file `st` describes.
pub fn of_stat(st: &libc::stat) -> Origin {
    of_file((st.st_dev as u64, st.st_ino as u64))
}

/// The origin of the data read from the open descriptor `fd`.
pub fn of_descriptor(fd: libc::c_int) -> Origin {
    if STAGE.get().is_none() {
        return Origin::Outside;
    }
    stat_of(fd).map_or(Origin::Outside, |st| of_stat(&st))
}

/// Whether `path` names the stage's output now. This only routes a load;
/// the load decides its origin from the descriptor it reads.
///
/// # Safety
/// `path` is null or a NUL-terminated string.
pub unsafe fn names_stage_file(path: *const std::ffi::c_char) -> bool {
    if STAGE.get().is_none() || path.is_null() {
        return false;
    }
    let fd = libc::open(path, libc::O_RDONLY | libc::O_NONBLOCK | libc::O_CLOEXEC);
    if fd < 0 {
        return false;
    }
    let origin = of_descriptor(fd);
    libc::close(fd);
    origin == Origin::Program
}

mod c_abi {
    /// # Safety
    /// `path` is a NUL-terminated string.
    #[no_mangle]
    pub unsafe extern "C" fn mlc_trust_stage_file(path: *const std::ffi::c_char) -> i32 {
        if path.is_null() {
            return 0;
        }
        std::ffi::CStr::from_ptr(path).to_str().is_ok_and(super::trust_stage_file) as i32
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

    fn status_of_child_with(name: &str, env: &[(&str, &str)]) -> i32 {
        let mut cmd = std::process::Command::new(std::env::current_exe().unwrap());
        cmd.args([&format!("origin::tests::{name}"), "--exact", "--ignored", "--test-threads=1"])
            .env(CHILD_ENV, "1")
            .env_remove(CODE_CHECKS_ENV)
            .env_remove("MORLOC_SKIP_UTF8_CHECK")
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null());
        for (k, v) in env {
            cmd.env(k, v);
        }
        cmd.status().unwrap().code().unwrap_or(-1)
    }

    fn mode(p: &std::path::Path, m: u32) {
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(p, std::fs::Permissions::from_mode(m)).unwrap();
    }

    fn test_root(tag: &str) -> std::path::PathBuf {
        let root = std::env::temp_dir().join(format!("morloc-origin-{tag}-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&root);
        std::fs::create_dir_all(&root).unwrap();
        root
    }

    #[test]
    fn a_stage_file_is_the_programs_own_only_where_the_nexus_puts_it() {
        let root = test_root("where");
        let code = status_of_child_with("child_trusts_only_a_private_stage_file", &[("MORLOC_TMPDIR", root.to_str().unwrap())]);
        let _ = std::fs::remove_dir_all(&root);
        assert_eq!(code, 0);
    }

    #[test]
    #[ignore]
    fn child_trusts_only_a_private_stage_file() {
        if std::env::var_os(CHILD_ENV).is_none() {
            return;
        }
        let root = std::path::PathBuf::from(std::env::var("MORLOC_TMPDIR").unwrap());
        let stage = |dir: &str, name: &str, dir_mode: u32| {
            let d = root.join(dir);
            std::fs::create_dir_all(&d).unwrap();
            mode(&d, dir_mode);
            let f = d.join(name);
            if !name.starts_with("link") {
                std::fs::write(&f, b"x").unwrap();
                mode(&f, 0o600);
            }
            f
        };
        let s = |p: &std::path::Path| p.to_str().unwrap().to_string();
        let outside = stage("elsewhere", "stream.pkt", 0o700);
        let open_dir = stage("morloc-stage-open01", "stream.pkt", 0o755);
        let link = root.join("morloc-stage-link01").join("stream.pkt");
        stage("morloc-stage-link01", "link", 0o700);
        std::os::unix::fs::symlink(&outside, &link).unwrap();
        let linked = stage("morloc-stage-hard01", "stream.pkt", 0o700);
        std::fs::hard_link(&linked, root.join("morloc-stage-hard01").join("twin")).unwrap();
        let writable = stage("morloc-stage-wrt001", "stream.pkt", 0o700);
        mode(&writable, 0o620);
        let misnamed = stage("morloc-stage-name01", "other.pkt", 0o700);
        let short = stage("morloc-stage-x", "stream.pkt", 0o700);
        let saved = stage("morloc-stage-good01", "value.pkt", 0o700);
        let second = stage("morloc-stage-good02", "stream.pkt", 0o700);
        let refused = [&outside, &open_dir, &link, &linked, &writable, &misnamed, &short]
            .iter()
            .all(|p| !trust_stage_file(&s(p)));
        let opened = |p: &std::path::Path| {
            let f = std::fs::File::open(p).unwrap();
            of_descriptor(std::os::fd::AsRawFd::as_raw_fd(&f))
        };
        let ok = refused
            && STAGE.get().is_none()
            && trust_stage_file(&s(&saved))
            && opened(&saved) == Origin::Program
            && opened(&outside) == Origin::Outside
            && opened(&link) == Origin::Outside
            && !trust_stage_file(&s(&second))
            && opened(&second) == Origin::Outside;
        unsafe { libc::_exit(if ok { 0 } else { 1 }) };
    }

    #[test]
    fn bytes_are_trusted_by_the_descriptor_they_were_read_from_never_by_their_name() {
        let root = test_root("toctou");
        let code = status_of_child_with(
            "child_reads_bytes_by_their_descriptor",
            &[("MORLOC_TMPDIR", root.to_str().unwrap()), ("MORLOC_SKIP_UTF8_CHECK", "1")],
        );
        let _ = std::fs::remove_dir_all(&root);
        assert_eq!(code, 0);
    }

    #[test]
    #[ignore]
    fn child_reads_bytes_by_their_descriptor() {
        if std::env::var_os(CHILD_ENV).is_none() {
            return;
        }
        let _g = crate::init_test_shm();
        let root = std::path::PathBuf::from(std::env::var("MORLOC_TMPDIR").unwrap());
        let dir = root.join("morloc-stage-abc123");
        std::fs::create_dir_all(&dir).unwrap();
        mode(&dir, 0o700);
        let saved = dir.join("value.pkt");
        let other = root.join("other.pkt");
        let schema = std::ffi::CString::new("as").unwrap();
        let ok = unsafe {
            let mut err: *mut std::ffi::c_char = std::ptr::null_mut();
            let cs = crate::ffi::parse_schema(schema.as_ptr(), &mut err);
            let v = crate::json::read_json_with_schema(r#"["okay", "xyzQxyz"]"#, &parse_schema("as").unwrap()).unwrap();
            let f = std::fs::File::create(&saved).unwrap();
            assert!(crate::intrinsics::mlc_write_voidstar_data_packet_to_fd(v as *const _, cs, 0, std::os::fd::AsRawFd::as_raw_fd(&f), &mut err) > 0);
            drop(f);
            let mut bytes = std::fs::read(&saved).unwrap();
            let at = bytes.windows(7).position(|w| w == b"xyzQxyz").unwrap();
            bytes[at + 3] = 0xff;
            std::fs::write(&saved, &bytes).unwrap();
            std::fs::write(&other, &bytes).unwrap();
            mode(&saved, 0o600);
            let load_arg = |p: &std::path::Path| {
                let value = std::ffi::CString::new(p.to_str().unwrap()).unwrap();
                let arg = crate::cli::initialize_positional(value.into_raw());
                let mut e: *mut std::ffi::c_char = std::ptr::null_mut();
                let got = crate::cli::parse_cli_data_argument(std::ptr::null_mut(), arg, cs, &mut e);
                let msg = if e.is_null() { String::new() } else { std::ffi::CStr::from_ptr(e).to_string_lossy().into_owned() };
                (!got.is_null(), msg)
            };
            let trusted = trust_stage_file(saved.to_str().unwrap());
            let (own, _) = load_arg(&saved);
            let (theirs, why) = load_arg(&other);
            let data = libc::malloc(bytes.len()) as *mut u8;
            std::ptr::copy_nonoverlapping(bytes.as_ptr(), data, bytes.len());
            let named = std::ffi::CString::new(saved.to_str().unwrap()).unwrap();
            let mut e: *mut std::ffi::c_char = std::ptr::null_mut();
            let swapped = crate::cli::load_morloc_data_file(named.as_ptr(), data, bytes.len(), cs, &mut e);
            let swapped_why = if e.is_null() { String::new() } else { std::ffi::CStr::from_ptr(e).to_string_lossy().into_owned() };
            trusted && own && !theirs && why.contains("outside") && swapped.is_null() && swapped_why.contains("outside")
        };
        unsafe { libc::_exit(if ok { 0 } else { 1 }) };
    }

    #[test]
    fn a_call_packet_read_from_a_file_passes_every_argument_as_outside_data() {
        let root = test_root("call");
        let code = status_of_child_with("child_reads_a_call_from_a_file_as_outside_data", &[("MORLOC_TMPDIR", root.to_str().unwrap())]);
        let _ = std::fs::remove_dir_all(&root);
        assert_eq!(code, 0);
    }

    #[test]
    #[ignore]
    fn child_reads_a_call_from_a_file_as_outside_data() {
        if std::env::var_os(CHILD_ENV).is_none() {
            return;
        }
        let _g = crate::init_test_shm();
        let root = std::path::PathBuf::from(std::env::var("MORLOC_TMPDIR").unwrap());
        let ok = unsafe {
            let cschema = |t: &str| {
                let c = std::ffi::CString::new(t).unwrap();
                crate::ffi::parse_schema(c.as_ptr(), &mut std::ptr::null_mut())
            };
            let packet_at = |p: *mut u8| {
                let h = &*(p as *const morloc_runtime_types::packet::PacketHeader);
                std::slice::from_raw_parts(p, 32 + h.offset as usize + h.length as usize).to_vec()
            };
            let read = |packet: &[u8], cs| {
                let mut e: *mut std::ffi::c_char = std::ptr::null_mut();
                let got = crate::packet_ffi::get_morloc_data_packet_value(packet.as_ptr(), cs, &mut e);
                let msg = if e.is_null() { String::new() } else { std::ffi::CStr::from_ptr(e).to_string_lossy().into_owned() };
                (!got.is_null(), msg)
            };
            // A file argument marked as the program's, of the wrong length.
            let three = cschema("a:3i4");
            let file = root.join("arg.mpk");
            std::fs::write(&file, two_ints().1).unwrap();
            let path = std::ffi::CString::new(file.to_str().unwrap()).unwrap();
            let marked = packet_at(crate::packet_ffi::make_program_file_packet(
                path.as_ptr(), three, morloc_runtime_types::packet::PACKET_FORMAT_MSGPACK,
            ));
            // An inline argument holding a Str that is not text.
            let strs = cschema("as");
            let v = crate::json::read_json_with_schema(r#"["okay", "xyzQxyz"]"#, &parse_schema("as").unwrap()).unwrap();
            let inline_file = root.join("inline.pkt");
            let f = std::fs::File::create(&inline_file).unwrap();
            assert!(crate::intrinsics::mlc_write_voidstar_data_packet_to_fd(v as *const _, strs, 0, std::os::fd::AsRawFd::as_raw_fd(&f), &mut std::ptr::null_mut()) > 0);
            drop(f);
            let mut inline = std::fs::read(&inline_file).unwrap();
            let at = inline.windows(7).position(|w| w == b"xyzQxyz").unwrap();
            inline[at + 3] = 0xff;
            let before = (read(&marked, three).0, read(&inline, strs).0);
            let call = morloc_runtime_types::packet::make_local_call_packet(0, &[marked.clone(), inline.clone()]);
            let mut spilled = Vec::new();
            let mut spill = |i: usize, arg: &[u8]| {
                let p = root.join(format!("spilled-{i}.pkt"));
                std::fs::write(&p, arg).unwrap();
                spilled.push(i);
                Ok(p.to_str().unwrap().to_string())
            };
            let admitted = morloc_runtime_types::packet::call_as_outside(&call, &mut spill).unwrap();
            let header = morloc_runtime_types::packet::validate_packet(&admitted).unwrap();
            let spans: Vec<_> = morloc_runtime_types::packet::CallArgs::over(&admitted, &header).map(|s| s.unwrap()).collect();
            let (file_arg, file_why) = read(&admitted[spans[0].clone()], three);
            let (inline_arg, inline_why) = read(&admitted[spans[1].clone()], strs);
            before == (true, true)
                && spilled == vec![1]
                && !file_arg && !file_why.is_empty()
                && !inline_arg && inline_why.contains("outside")
        };
        unsafe { libc::_exit(if ok { 0 } else { 1 }) };
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
