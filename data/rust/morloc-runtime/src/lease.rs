// FORK-15: references a forked child holds, recorded in a file the child keeps
// locked; whoever takes the lock once the child is gone releases them.

use std::io::Read;
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::{FileExt, MetadataExt};
use std::os::unix::io::AsRawFd;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};

use crate::shm::RelPtr;

static CREATED: AtomicBool = AtomicBool::new(false);
static FORKED: AtomicBool = AtomicBool::new(false);

// FORK-16: a process that has forked reclaims what its children leave.
pub(crate) fn note_forked() {
    FORKED.store(true, Ordering::Relaxed);
}
static SEQ: AtomicU64 = AtomicU64::new(0);
static LAST_RECLAIM_MS: AtomicU64 = AtomicU64::new(0);

const RECLAIM_INTERVAL_MS: u64 = 250;
// FORK-15: the child's token, written by the child, fills the first line.
const TOKEN_BYTES: u64 = 32;

pub(crate) struct Paths {
    dir: PathBuf,
    staging: PathBuf,
    basename: String,
}

/// A lease created and locked before a fork, written and placed after it.
pub(crate) struct Staged {
    file: std::fs::File,
    staged: PathBuf,
    name: String,
    dir: PathBuf,
    basename: String,
}

// FORK-15: a lock a forked child inherits with the open file description;
// a network filesystem emulates the lock per process, which a child does not
// inherit.
fn holds_inherited_locks(dir: &Path) -> bool {
    let Ok(c) = std::ffi::CString::new(dir.as_os_str().as_bytes()) else { return false };
    let mut st: libc::statfs = unsafe { std::mem::zeroed() };
    if unsafe { libc::statfs(c.as_ptr(), &mut st) } != 0 {
        return false;
    }
    #[cfg(target_os = "linux")]
    {
        const REMOTE: &[i64] = &[
            0x6969,             // NFS
            0x517B,             // SMB
            0xFF534D42u32 as i64, // CIFS
            0xFE534D42u32 as i64, // SMB2
            0x00C36400,         // Ceph
            0x65735546,         // FUSE
            0x01021997,         // 9P
            0x5346414F,         // AFS
            0x0BD00BD0,         // Lustre
            0x47504653,         // GPFS
            0x01161970,         // GFS2
        ];
        !REMOTE.contains(&(st.f_type as i64))
    }
    #[cfg(target_os = "macos")]
    {
        st.f_flags & (libc::MNT_LOCAL as u32) != 0
    }
}

fn local_dir(dir: PathBuf) -> Option<PathBuf> {
    std::fs::create_dir_all(&dir).ok()?;
    holds_inherited_locks(&dir).then_some(dir)
}

/// Where the leases of the shared namespace named `basename` live: the run
/// directory, or else the system temp directory, whichever keeps a forked
/// child's lock.
fn locate(run_dir: Option<String>, basename: &str) -> Option<PathBuf> {
    if let Some(root) = run_dir.filter(|d| !d.is_empty()) {
        if let Some(dir) = local_dir(Path::new(&root).join("leases")) {
            return Some(dir);
        }
    }
    local_dir(temp_lease_dir(basename))
}

fn temp_lease_dir(basename: &str) -> PathBuf {
    let tag: String = basename.chars().map(|c| if c.is_ascii_alphanumeric() { c } else { '_' }).collect();
    std::env::temp_dir().join(format!("morloc-leases-{}", tag))
}

// FORK-15: read before the fork handler takes the locks guarding these.
pub(crate) fn paths() -> Option<Paths> {
    let basename = crate::shm::get_common_basename();
    if basename.is_empty() {
        return None;
    }
    let dir = locate(crate::shm::get_fallback_dir(), &basename)?;
    Some(Paths { staging: dir.join(".staging"), dir, basename })
}

// FORK-15: created and locked where no reclaimer looks.
pub(crate) fn stage(paths: &Paths) -> std::io::Result<Staged> {
    std::fs::create_dir_all(&paths.staging)?;
    let name = format!(
        "lease-{:016x}-{}",
        morloc_runtime_types::process::token(),
        SEQ.fetch_add(1, Ordering::Relaxed)
    );
    let staged = paths.staging.join(&name);
    let file = std::fs::OpenOptions::new().read(true).write(true).create_new(true).open(&staged)?;
    if unsafe { libc::flock(file.as_raw_fd(), libc::LOCK_EX) } != 0 {
        let e = std::io::Error::last_os_error();
        let _ = std::fs::remove_file(&staged);
        return Err(e);
    }
    Ok(Staged { file, staged, name, dir: paths.dir.clone(), basename: paths.basename.clone() })
}

impl Staged {
    // FORK-15: in the child; its token marks the lease live while it runs,
    // whatever it does with its descriptors.
    pub(crate) fn mark_child(&self) {
        let line = format!("{:016x}", morloc_runtime_types::process::token());
        let mut buf = [b' '; TOKEN_BYTES as usize];
        buf[..line.len()].copy_from_slice(line.as_bytes());
        buf[TOKEN_BYTES as usize - 1] = b'\n';
        let _ = self.file.write_at(&buf, 0);
    }

    // FORK-15: in the parent, once the locks are released. A name already
    // taken is never replaced.
    pub(crate) fn place(self, rels: &[RelPtr]) {
        if rels.is_empty() {
            let _ = std::fs::remove_file(&self.staged);
            return;
        }
        let mut text = format!("{}\n", self.basename);
        for rel in rels {
            text.push_str(&format!("{}\n", rel));
        }
        if self.file.write_all_at(text.as_bytes(), TOKEN_BYTES).is_err() {
            return;
        }
        let mut name = self.name.clone();
        for _ in 0..8 {
            match std::fs::hard_link(&self.staged, self.dir.join(&name)) {
                Ok(()) => {
                    let _ = std::fs::remove_file(&self.staged);
                    CREATED.store(true, Ordering::Relaxed);
                    return;
                }
                Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => {
                    name = format!("{}-{}", self.name, SEQ.fetch_add(1, Ordering::Relaxed));
                }
                Err(_) => return,
            }
        }
    }
}

/// Release the references of every lease whose holder is gone, and remove
/// the temp directories of processes that are gone. Returns how many leases
/// were released.
pub fn reclaim() -> usize {
    reclaim_temp_dirs();
    let Some(paths) = paths() else { return 0 };
    let Ok(entries) = std::fs::read_dir(&paths.dir) else { return 0 };
    let mut released = 0;
    for entry in entries.flatten() {
        if !entry.file_name().to_string_lossy().starts_with("lease-") {
            continue;
        }
        if release_if_free(&entry.path(), &paths.basename) {
            released += 1;
        }
    }
    released
}

// FORK-16
fn reclaim_temp_dirs() {
    // Outside a run the root is a process's own.
    if crate::intrinsics::run_dir().is_none() {
        return;
    }
    let Ok(entries) = std::fs::read_dir(crate::intrinsics::temp_root()) else { return };
    let own = morloc_runtime_types::process::token();
    for entry in entries.flatten() {
        let name = entry.file_name();
        let Some(hex) = name.to_str().and_then(|n| n.strip_prefix("tmp-")) else { continue };
        let Ok(token) = u64::from_str_radix(hex, 16) else { continue };
        if hex.len() == 16 && token != own && !morloc_runtime_types::process::token_alive(token) {
            let _ = std::fs::remove_dir_all(entry.path());
        }
    }
}

fn release_if_free(path: &Path, basename: &str) -> bool {
    let Ok(mut file) = std::fs::OpenOptions::new().read(true).write(true).open(path) else { return false };
    if unsafe { libc::flock(file.as_raw_fd(), libc::LOCK_EX | libc::LOCK_NB) } != 0 {
        return false;
    }
    // FORK-15: another reclaimer may have released and removed it meanwhile.
    let same = match (file.metadata(), std::fs::metadata(path)) {
        (Ok(held), Ok(named)) => held.ino() == named.ino() && held.dev() == named.dev(),
        _ => false,
    };
    if !same {
        return false;
    }
    let mut text = String::new();
    if file.read_to_string(&mut text).is_err() || text.len() < TOKEN_BYTES as usize {
        return false;
    }
    let (token_line, rest) = text.split_at(TOKEN_BYTES as usize);
    let token = u64::from_str_radix(token_line.trim(), 16).ok();
    if token.is_some_and(morloc_runtime_types::process::token_alive) {
        return false;
    }
    let mut lines = rest.lines();
    // FORK-15: a lease of an earlier shared namespace names nothing live.
    if lines.next() == Some(basename) {
        for rel in lines.filter_map(|l| l.parse::<RelPtr>().ok()) {
            if let Ok(abs) = crate::shm::rel2abs(rel) {
                crate::shm::free_uncounted(abs);
            }
        }
    }
    std::fs::remove_file(path).is_ok()
}

// FORK-15: a process that made leases reclaims them at dispatch ends, when
// idle, and before giving up on an allocation.
pub fn reclaim_if_due() {
    if !CREATED.load(Ordering::Relaxed) && !FORKED.load(Ordering::Relaxed) {
        return;
    }
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map_or(0, |d| d.as_millis() as u64);
    let last = LAST_RECLAIM_MS.load(Ordering::Relaxed);
    if now.saturating_sub(last) < RECLAIM_INTERVAL_MS
        || LAST_RECLAIM_MS.compare_exchange(last, now, Ordering::Relaxed, Ordering::Relaxed).is_err()
    {
        return;
    }
    reclaim();
}

pub(crate) fn any_created() -> bool {
    CREATED.load(Ordering::Relaxed)
}

#[no_mangle]
pub extern "C" fn morloc_reclaim_all() {
    reclaim();
}

#[no_mangle]
pub extern "C" fn morloc_reclaim_leases() {
    reclaim_if_due();
}

/// Remove the temp-directory leases of the current shared namespace; the
/// run directory's go with it.
#[no_mangle]
pub extern "C" fn morloc_remove_leases() {
    let basename = crate::shm::get_common_basename();
    if !basename.is_empty() {
        let _ = std::fs::remove_dir_all(temp_lease_dir(&basename));
    }
}
