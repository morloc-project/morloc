//! The fork side (side.md): the record that lets a forked child's batches
//! reach an in-pool accumulator, a native gather's list or a fold's slots,
//! through a side file.

use std::collections::HashMap;
use std::ffi::{c_char, c_void, CString};
use std::ptr;
use std::sync::atomic::{AtomicBool, AtomicI64, AtomicPtr, AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

use crate::error::MorlocError;
use crate::schema::Schema;
use crate::shm::AbsPtr;
use morloc_runtime_types::fork_generation::generation;

const NATIVE: i64 = 0;
const SWITCHING: i64 = 1;
const SIDE: i64 = 2;
const FAILED: i64 = 3;
const TAKEN: i64 = 4;

const OPEN: u8 = 0;
const CLOSED: u8 = 1;
// SIDE-12: marked in place, so the mark needs no room the put lacked.
const MARKED: u8 = 2;

// SIDE-10: the state byte, the committed length and its complement.
const HEADER: u64 = 17;

const POISON: u64 = u64::MAX;

/// What an entry accumulates: its route (FOLD-1) and what its errors name.
#[derive(Clone, Copy, PartialEq)]
pub enum Kind {
    Gather,
    Fold,
}

impl Kind {
    fn noun(self) -> &'static str {
        match self {
            Kind::Gather => "gather",
            Kind::Fold => "fold",
        }
    }
}

pub struct SideEntry {
    kind: Kind,
    owner: u64,
    temp_owner: u64,
    born: u64,
    state: AtomicI64,
    path: AtomicPtr<String>,
    lock: Mutex<()>,
    native: AtomicPtr<c_void>,
    // SIDE-12
    failed: AtomicBool,
}

#[derive(Default)]
pub(crate) struct Sides {
    entries: HashMap<i64, Arc<SideEntry>>,
    // SIDE-11: the ordinal gather ids are numbered under, the next number,
    // and the ordinals drawn before it, each with its next number then.
    ordinal: u64,
    drawn_in: u64,
    next: u64,
    drawn: Vec<(u64, u64)>,
}

const ORDINAL_MAX: u64 = (1 << 31) - 1;
const NUMBER_MAX: u64 = u32::MAX as u64;

impl Sides {
    // SIDE-11: a child draws its own ordinal; ids its parent made are inherited.
    fn fresh_id(&mut self) -> Result<i64, MorlocError> {
        if self.ordinal == 0 || self.drawn_in != generation() || self.next > NUMBER_MAX {
            let o = crate::shm::draw_ordinal()?;
            if o == 0 || o > ORDINAL_MAX {
                return Err(err("this program has numbered more processes than a gather id holds"));
            }
            if self.ordinal != 0 {
                self.drawn.push((self.ordinal, self.next));
            }
            self.ordinal = o;
            self.drawn_in = generation();
            self.next = 1;
        }
        let id = -(((self.ordinal << 32) | self.next) as i64);
        self.next += 1;
        Ok(id)
    }

    fn made_here(&self, id: i64) -> bool {
        let Some(m) = id.checked_neg().map(|m| m as u64) else { return false };
        let (o, n) = (m >> 32, m & NUMBER_MAX);
        (o == self.ordinal && n < self.next) || self.drawn.iter().any(|&(d, end)| d == o && n < end)
    }
}

// SIDE-2: held across fork, so a child sees every entry its parent made.
// SIDE-4: gather and fold ids are negative, apart from every stream handle.
pub(crate) static REGISTRY: crate::fork_policy::Held<Option<Sides>> = crate::fork_policy::Held::new(19, None);

// SIDE-2, SIDE-3
static FORKING: AtomicU64 = AtomicU64::new(0);

pub(crate) fn registry_lock() -> crate::fork_policy::HeldGuard<'static, Option<Sides>> {
    REGISTRY.lock()
}

fn err(msg: &str) -> MorlocError {
    MorlocError::Other(msg.to_string())
}

fn late(noun: &str) -> MorlocError {
    MorlocError::Other(format!("a sink was called after its {noun} finished"))
}

fn torn() -> MorlocError {
    err("a write to the side file failed while committing a batch")
}

fn header(committed: u64) -> [u8; 16] {
    let mut h = [0u8; 16];
    h[..8].copy_from_slice(&committed.to_le_bytes());
    h[8..].copy_from_slice(&(!committed).to_le_bytes());
    h
}

// The state byte and the committed length; a length whose complement does
// not match was torn by a failed commit.
fn read_header(fd: i32) -> Result<(u8, u64), MorlocError> {
    let mut h = [0u8; HEADER as usize];
    pread_exact(fd, &mut h, 0)?;
    let word = |at: usize| {
        let mut w = [0u8; 8];
        w.copy_from_slice(&h[at..at + 8]);
        u64::from_le_bytes(w)
    };
    let committed = word(1);
    if word(9) != !committed || committed < HEADER {
        return Err(torn());
    }
    Ok((h[0], committed))
}

fn commit(fd: i32, committed: u64) -> Result<(), MorlocError> {
    let h = header(committed);
    #[cfg(test)]
    match tests::FAIL_COMMIT.load(Ordering::SeqCst) {
        1 => return Err(err("commit failed")),
        2 => {
            crate::stream_format::pwrite_all_fd(fd, &h[..4], 1)?;
            return Err(err("commit failed"));
        }
        _ => {}
    }
    crate::stream_format::pwrite_all_fd(fd, &h, 1)
}

fn io() -> MorlocError {
    MorlocError::Io(std::io::Error::last_os_error())
}

fn lookup(id: i64) -> Option<Arc<SideEntry>> {
    registry_lock().as_ref().and_then(|m| m.entries.get(&id).cloned())
}

// SIDE-11: an id with no entry here was taken, or names another process's.
fn missing(id: i64, noun: &str) -> MorlocError {
    let here = registry_lock().as_ref().is_some_and(|m| m.made_here(id));
    if id >= 0 || here {
        late(noun)
    } else {
        MorlocError::Other(format!("a native {noun} handle reached another process"))
    }
}

fn entry(id: i64, noun: &str) -> Result<Arc<SideEntry>, MorlocError> {
    lookup(id).ok_or_else(|| missing(id, noun))
}

fn mismatched(e: &SideEntry, kind: Kind) -> MorlocError {
    MorlocError::Other(format!("internal error: a {} call reached a {} entry", kind.noun(), e.kind.noun()))
}

// SIDE-4: a call of one kind never reads another kind's native pointer.
fn entry_of(id: i64, kind: Kind) -> Result<Arc<SideEntry>, MorlocError> {
    let e = entry(id, kind.noun())?;
    if e.kind != kind {
        return Err(mismatched(&e, kind));
    }
    Ok(e)
}

impl Drop for SideEntry {
    fn drop(&mut self) {
        let p = *self.path.get_mut();
        if !p.is_null() {
            drop(unsafe { Box::from_raw(p) });
        }
    }
}

impl SideEntry {
    // SIDE-1: set once, before the state word says SIDE.
    fn path(&self) -> Option<&str> {
        let p = self.path.load(Ordering::Acquire);
        unsafe { p.as_ref() }.map(|s| s.as_str())
    }

    fn create_side(&self) -> Result<(), MorlocError> {
        let path = crate::intrinsics::make_temp(self.temp_owner, self.born)?;
        std::fs::write(&path, [&[OPEN][..], &header(HEADER)].concat()).map_err(MorlocError::Io)?;
        let fresh = Box::into_raw(Box::new(path));
        if self.path.compare_exchange(ptr::null_mut(), fresh, Ordering::AcqRel, Ordering::Acquire).is_err() {
            drop(unsafe { Box::from_raw(fresh) });
        }
        Ok(())
    }

    // SIDE-10: every write and the take lock a description of their own.
    fn lock_file(&self) -> Result<SideFd, MorlocError> {
        let path = self.path().ok_or_else(|| err("side file: no path"))?;
        let c = CString::new(path).map_err(|_| err("side file: path contained NUL"))?;
        let fd = unsafe { libc::open(c.as_ptr(), libc::O_RDWR | libc::O_CLOEXEC) };
        if fd < 0 {
            let e = std::io::Error::last_os_error();
            return Err(if e.kind() == std::io::ErrorKind::NotFound { late(self.kind.noun()) } else { MorlocError::Io(e) });
        }
        let f = SideFd(fd);
        loop {
            if unsafe { libc::flock(fd, libc::LOCK_EX) } == 0 {
                return Ok(f);
            }
            let e = std::io::Error::last_os_error();
            if e.kind() != std::io::ErrorKind::Interrupted {
                return Err(MorlocError::Io(e));
            }
        }
    }

    fn write_record(&self, bytes: &[u8]) -> Result<(), MorlocError> {
        self.append(bytes.len() as u64, bytes)
    }

    // SIDE-10: a record counts once the committed length covers it; a
    // failed write leaves an uncommitted tail the next write overwrites.
    fn append(&self, len: u64, body: &[u8]) -> Result<(), MorlocError> {
        let f = self.lock_file()?;
        let (state, committed) = read_header(f.0)?;
        if state == MARKED {
            return Err(err("an earlier put in a forked child raised, so this fold or gather has failed"));
        }
        if state != OPEN {
            return Err(late(self.kind.noun()));
        }
        #[cfg(test)]
        tests::hook(&tests::AFTER_APPEND_CHECK);
        let end = committed
            .checked_add(8 + body.len() as u64)
            .ok_or_else(|| err("side file: too large"))?;
        crate::stream_format::pwrite_all_fd(f.0, &len.to_le_bytes(), committed)?;
        crate::stream_format::pwrite_all_fd(f.0, body, committed + 8)?;
        commit(f.0, end)
    }

    // SIDE-7: only the owner's take closes the file. Once it is marked
    // closed no write appends (SIDE-10), so the reader needs no lock.
    fn close(&self) -> Result<Reader, MorlocError> {
        let r = self.mark_closed();
        if let Some(path) = self.path() {
            crate::intrinsics::unlink_temp(path);
        }
        r
    }

    // SIDE-12: a file already closed was taken; the put was late.
    fn mark_failed(&self) -> Result<(), MorlocError> {
        let f = self.lock_file()?;
        let (state, _) = read_header(f.0)?;
        if state != OPEN {
            return Ok(());
        }
        if unsafe { libc::pwrite(f.0, [MARKED].as_ptr() as *const c_void, 1, 0) } != 1 {
            return Err(io());
        }
        Ok(())
    }

    fn mark_closed(&self) -> Result<Reader, MorlocError> {
        let f = self.lock_file()?;
        let h = read_header(f.0);
        if unsafe { libc::pwrite(f.0, [CLOSED].as_ptr() as *const c_void, 1, 0) } != 1 {
            return Err(io());
        }
        let (state, committed) = h?;
        let size = unsafe { libc::lseek(f.0, 0, libc::SEEK_END) };
        if size < 0 {
            return Err(io());
        }
        if committed > size as u64 {
            return Err(torn());
        }
        unsafe { libc::flock(f.0, libc::LOCK_UN) };
        let mut r = Reader { file: Some(f), at: HEADER, end: committed };
        // SIDE-12: a poison record names the failure better than the mark.
        if state == MARKED {
            while r.next()?.is_some() {}
            return Err(err("a put in a forked child raised, so its batch is missing; the producer went on without it"));
        }
        Ok(r)
    }

    // SIDE-1: one CAS to TAKEN, after any prepare settles the word. FOLD-2:
    // the CAS and the clearing of the native pointer are one locked step.
    // The state taken from, and the native pointer it held.
    fn end(&self) -> (i64, *mut c_void) {
        loop {
            let st = self.state.load(Ordering::Acquire);
            if st == SWITCHING {
                std::thread::yield_now();
                continue;
            }
            #[cfg(test)]
            tests::hook(&tests::AFTER_TAKE_LOAD);
            let _g = self.lock.lock().unwrap_or_else(|p| p.into_inner());
            if st == TAKEN || self.state.compare_exchange(st, TAKEN, Ordering::AcqRel, Ordering::Acquire).is_ok() {
                return (st, self.native.swap(ptr::null_mut(), Ordering::AcqRel));
            }
        }
    }
}

fn pread_exact(fd: i32, buf: &mut [u8], mut offset: u64) -> Result<(), MorlocError> {
    let mut at = 0usize;
    while at < buf.len() {
        let n = unsafe { libc::pread(fd, buf[at..].as_mut_ptr() as *mut c_void, buf.len() - at, offset as libc::off_t) };
        if n < 0 {
            if std::io::Error::last_os_error().kind() == std::io::ErrorKind::Interrupted {
                continue;
            }
            return Err(io());
        }
        if n == 0 {
            return Err(err("side file: read past its end"));
        }
        at += n as usize;
        offset += n as u64;
    }
    Ok(())
}

// SIDE-10: a side file's descriptor; closing it releases its flock.
struct SideFd(i32);

impl Drop for SideFd {
    fn drop(&mut self) {
        unsafe { libc::close(self.0) };
    }
}

/// A gather's or a fold's entry, holding `native`.
pub fn new(native: *mut c_void, kind: Kind) -> Result<i64, MorlocError> {
    let e = Arc::new(SideEntry {
        kind,
        owner: generation(),
        temp_owner: crate::intrinsics::current_temp_owner(),
        born: crate::intrinsics::next_dispatch_id(),
        state: AtomicI64::new(NATIVE),
        path: AtomicPtr::new(ptr::null_mut()),
        lock: Mutex::new(()),
        native: AtomicPtr::new(native),
        failed: AtomicBool::new(false),
    });
    // SIDE-2: made only with no fork in progress, waiting with the lock released.
    loop {
        let mut reg = registry_lock();
        if FORKING.load(Ordering::SeqCst) == 0 {
            let sides = reg.get_or_insert_with(Sides::default);
            let id = sides.fresh_id()?;
            sides.entries.insert(id, e);
            return Ok(id);
        }
        drop(reg);
        std::thread::yield_now();
    }
}

fn remove_own(id: i64) -> Option<Arc<SideEntry>> {
    let mut reg = registry_lock();
    let m = reg.as_mut()?;
    if !m.entries.get(&id).is_some_and(|e| e.owner == generation()) {
        return None;
    }
    m.entries.remove(&id)
}

pub fn drop_entry(id: i64) {
    drop_native(id, Kind::Gather);
}

/// The native pointer of an entry ended unread; null if there was none.
pub(crate) fn drop_native(id: i64, kind: Kind) -> *mut c_void {
    if let Some(e) = lookup(id).filter(|e| e.kind != kind) {
        morloc_runtime_types::panic::fatal(&format!("morloc: {}", mismatched(&e, kind)));
    }
    let Some(e) = remove_own(id) else { return ptr::null_mut() };
    let (st, native) = e.end();
    if st == SIDE {
        let _ = e.close();
    }
    native
}

// FOLD-1: a fold's owner puts natively in every state but TAKEN.
fn route_entry(e: &SideEntry) -> Result<bool, MorlocError> {
    let st = e.state.load(Ordering::Acquire);
    if st == TAKEN {
        return Err(late(e.kind.noun()));
    }
    if generation() == e.owner {
        return match st {
            _ if e.kind == Kind::Fold => Ok(true),
            NATIVE | SWITCHING => Ok(true),
            SIDE => Ok(false),
            _ => Err(err("the side file for a forked child could not be made")),
        };
    }
    match st {
        SIDE => Ok(false),
        FAILED => Err(err("a forked child's batch has no side file: making it failed")),
        _ => morloc_runtime_types::panic::fatal(
            "morloc: a native accumulator was used in a process it was not forked into",
        ),
    }
}

/// SIDE-5, FOLD-1: `f` applied to the native pointer under the entry lock;
/// None when the put goes to the side file. A null pointer in the owner is
/// a late put.
pub(crate) fn with_native<R>(id: i64, kind: Kind, f: impl FnOnce(*mut c_void) -> R) -> Result<Option<R>, MorlocError> {
    let e = entry_of(id, kind)?;
    if !route_entry(&e)? {
        return Ok(None);
    }
    let _g = e.lock.lock().unwrap_or_else(|p| p.into_inner());
    let c = e.native.load(Ordering::Acquire);
    if c.is_null() {
        return Err(late(e.kind.noun()));
    }
    Ok(Some(f(c)))
}

/// SIDE-4: whether a gather's put holds its batch natively.
pub fn route(id: i64) -> Result<bool, MorlocError> {
    entry_of(id, Kind::Gather).and_then(|e| route_entry(&e))
}

pub type NativeFn = unsafe extern "C" fn(native: *mut c_void, arg: *mut c_void);

/// SIDE-10: a batch with no schema (no wire form) poisons the side file.
///
/// # Safety
/// `value` points at a value laid out as `schema` describes.
pub unsafe fn put_side(id: i64, schema: Option<&Schema>, value: AbsPtr) -> Result<(), MorlocError> {
    let e = entry(id, "gather or fold")?;
    if route_entry(&e)? && (e.kind == Kind::Fold || e.state.load(Ordering::Acquire) != SIDE) {
        return Err(err("a native put was sent to its side file"));
    }
    match schema {
        Some(schema) => e.write_record(&crate::voidstar::flatten_to_buffer(value, schema)?),
        None => {
            e.append(POISON, &[])?;
            Err(err("@fold: a forked child's batch has no wire form in its language"))
        }
    }
}

/// SIDE-1, SIDE-7, SIDE-8
pub fn take(id: i64) -> Result<Reader, MorlocError> {
    finish(id, Kind::Gather).1
}

/// The native pointer the take cleared (null if none), and the side records.
pub(crate) fn finish(id: i64, kind: Kind) -> (*mut c_void, Result<Reader, MorlocError>) {
    let noun = kind.noun();
    let twice = || MorlocError::Other(format!("a {noun} was finished twice"));
    let Some(e) = lookup(id) else { return (ptr::null_mut(), Err(twice())) };
    if e.kind != kind {
        return (ptr::null_mut(), Err(mismatched(&e, kind)));
    }
    if generation() != e.owner {
        return (ptr::null_mut(), Err(MorlocError::Other(format!("a forked child cannot finish its parent's {noun}"))));
    }
    let (st, native) = e.end();
    remove_own(id);
    let records = match st {
        NATIVE => Ok(Reader { file: None, at: 0, end: 0 }),
        SIDE => e.close(),
        TAKEN => Err(twice()),
        _ => Err(err("a forked child's batches were lost: the side file could not be made")),
    };
    // SIDE-12
    let records = records.and_then(|r| if e.failed.load(Ordering::Acquire) { Err(failed_put(noun)) } else { Ok(r) });
    (native, records)
}

fn failed_put(noun: &str) -> MorlocError {
    MorlocError::Other(format!(
        "a put into this {noun} raised, so its batch is missing; the producer went on without it"
    ))
}

/// SIDE-12: a put that raised fails its gather or fold. A late call,
/// or one for an entry this process cannot see, changes nothing.
pub fn fail(id: i64) {
    let Some(e) = lookup(id) else { return };
    if generation() == e.owner {
        let _g = e.lock.lock().unwrap_or_else(|p| p.into_inner());
        if e.state.load(Ordering::Acquire) != TAKEN {
            e.failed.store(true, Ordering::Release);
        }
    } else if e.state.load(Ordering::Acquire) == SIDE {
        if let Err(m) = e.mark_failed() {
            morloc_runtime_types::panic::fatal(&format!(
                "morloc: a put in a forked child raised, and marking its {} failed failed too: {m}",
                e.kind.noun()
            ));
        }
    }
}

// SIDE-2: called from the prepare handler before any held lock is taken.
pub(crate) fn prepare() {
    let me = generation();
    let mine: Vec<Arc<SideEntry>> = {
        let reg = registry_lock();
        FORKING.fetch_add(1, Ordering::SeqCst);
        reg.iter()
            .flat_map(|m| m.entries.values())
            .filter(|e| e.owner == me)
            .filter(|e| e.state.compare_exchange(NATIVE, SWITCHING, Ordering::AcqRel, Ordering::Acquire).is_ok())
            .cloned()
            .collect()
    };
    for e in &mine {
        let st = if e.create_side().is_ok() { SIDE } else { FAILED };
        e.state.store(st, Ordering::Release);
    }
    // SIDE-2: another thread's prepare may still be making a side file.
    loop {
        let unsettled = registry_lock()
            .iter()
            .flat_map(|m| m.entries.values())
            .any(|e| e.owner == me && e.state.load(Ordering::Acquire) == SWITCHING);
        if !unsettled {
            return;
        }
        std::thread::yield_now();
    }
}

pub(crate) fn after_fork_in_parent() {
    FORKING.fetch_sub(1, Ordering::SeqCst);
}

// SIDE-3
pub(crate) fn after_fork_in_child() {
    FORKING.store(0, Ordering::SeqCst);
}

/// Side records left to decode, read one at a time.
pub struct Reader {
    file: Option<SideFd>,
    at: u64,
    end: u64,
}

impl Reader {
    /// SIDE-10: a short last record fails the take.
    pub fn next(&mut self) -> Result<Option<Vec<u8>>, MorlocError> {
        let Some(f) = &self.file else { return Ok(None) };
        let left = self.end.saturating_sub(self.at);
        if left == 0 {
            return Ok(None);
        }
        let torn = || err("a forked writer died while writing a batch");
        if left < 8 {
            return Err(torn());
        }
        let mut head = [0u8; 8];
        pread_exact(f.0, &mut head, self.at)?;
        let n = u64::from_le_bytes(head);
        if n == POISON {
            return Err(err("a forked child could not send its batch: its type has no wire form in its language"));
        }
        if n > left - 8 {
            return Err(torn());
        }
        let mut body = vec![0u8; usize::try_from(n).map_err(|_| torn())?];
        pread_exact(f.0, &mut body, self.at + 8)?;
        self.at += 8 + n;
        Ok(Some(body))
    }
}

mod c_abi {
    use super::*;
    use crate::cschema::CSchema;

    /// SIDE-12: called by a binding whose put into `h` raised: a gather or
    /// fold id, or a stream handle (FOLD-9).
    #[no_mangle]
    pub extern "C" fn mlc_sink_failed(h: i64) {
        if h < 0 {
            fail(h);
        } else {
            crate::stream::fail_written(h, "a put into this stream raised, so its batch is missing; the producer went on without it");
        }
    }

    unsafe fn schema_of(s: *const CSchema, what: &str) -> Result<Schema, MorlocError> {
        if s.is_null() {
            return Err(err(&format!("{what}: null schema")));
        }
        Ok(CSchema::to_rust(s))
    }

    /// # Safety
    /// `errmsg` is a valid out-pointer.
    #[no_mangle]
    pub unsafe extern "C" fn mlc_side_new(native: *mut c_void, errmsg: *mut *mut c_char) -> i64 {
        crate::error::clear_errmsg(errmsg);
        match new(native, Kind::Gather) {
            Ok(id) => id,
            Err(e) => {
                crate::error::set_errmsg(errmsg, &e);
                -1
            }
        }
    }

    /// 1 if a gather's batch is held natively here, 0 if it goes to the
    /// side file, -1 on error. A native put may still find the side route.
    ///
    /// # Safety
    /// `errmsg` is a valid out-pointer.
    #[no_mangle]
    pub unsafe extern "C" fn mlc_side_route(id: i64, errmsg: *mut *mut c_char) -> i32 {
        crate::error::clear_errmsg(errmsg);
        match route(id) {
            Ok(native) => native as i32,
            Err(e) => {
                crate::error::set_errmsg(errmsg, &e);
                -1
            }
        }
    }

    /// 1 if `f` ran on the native pointer, 0 if the batch goes to the side
    /// file, -1 on error. `is_fold` names the fold in an error that finds no
    /// entry.
    ///
    /// # Safety
    /// `f` must accept the native pointer registered for `id` and `arg`.
    #[no_mangle]
    pub unsafe extern "C" fn mlc_side_with_native(id: i64, is_fold: i32, f: NativeFn, arg: *mut c_void, errmsg: *mut *mut c_char) -> i32 {
        crate::error::clear_errmsg(errmsg);
        let kind = if is_fold != 0 { Kind::Fold } else { Kind::Gather };
        match with_native(id, kind, |c| f(c, arg)) {
            Ok(ran) => ran.is_some() as i32,
            Err(e) => {
                crate::error::set_errmsg(errmsg, &e);
                -1
            }
        }
    }

    /// A NULL schema says the batch has no wire form: the side file is
    /// poisoned and the call fails.
    ///
    /// # Safety
    /// `value` points at a value laid out as `schema` describes.
    #[no_mangle]
    pub unsafe extern "C" fn mlc_side_put(id: i64, schema: *const CSchema, value: *const c_void, errmsg: *mut *mut c_char) -> i32 {
        crate::error::clear_errmsg(errmsg);
        let s = (!schema.is_null()).then(|| &CSchema::converted(schema).schema);
        let r = put_side(id, s, value as AbsPtr);
        match r {
            Ok(()) => 0,
            Err(e) => {
                crate::error::set_errmsg(errmsg, &e);
                -1
            }
        }
    }

    /// The reader over the side records, NULL only on error.
    ///
    /// # Safety
    /// `errmsg` is a valid out-pointer.
    #[no_mangle]
    pub unsafe extern "C" fn mlc_side_take(id: i64, errmsg: *mut *mut c_char) -> *mut c_void {
        crate::error::clear_errmsg(errmsg);
        match take(id) {
            Ok(reader) => Box::into_raw(Box::new(reader)) as *mut c_void,
            Err(e) => {
                crate::error::set_errmsg(errmsg, &e);
                ptr::null_mut()
            }
        }
    }

    /// The next side batch as a fresh SHM value, or NULL with `eof` set.
    ///
    /// # Safety
    /// `reader` came from `mlc_side_take` and was not freed.
    #[no_mangle]
    pub unsafe extern "C" fn mlc_side_next(reader: *mut c_void, schema: *const CSchema, eof: *mut i32, errmsg: *mut *mut c_char) -> *mut c_void {
        crate::error::clear_errmsg(errmsg);
        *eof = 0;
        let r = &mut *(reader as *mut Reader);
        let rec = match r.next() {
            Ok(Some(rec)) => rec,
            Ok(None) => {
                *eof = 1;
                return ptr::null_mut();
            }
            Err(e) => {
                crate::error::set_errmsg(errmsg, &e);
                return ptr::null_mut();
            }
        };
        match schema_of(schema, "mlc_side_next").and_then(|s| crate::voidstar::read_binary(&rec, &s)) {
            Ok(p) => p as *mut c_void,
            Err(e) => {
                crate::error::set_errmsg(errmsg, &e);
                ptr::null_mut()
            }
        }
    }

    /// # Safety
    /// As `mlc_side_next`; `reader` is not used again.
    #[no_mangle]
    pub unsafe extern "C" fn mlc_side_reader_free(reader: *mut c_void) {
        if !reader.is_null() {
            drop(Box::from_raw(reader as *mut Reader));
        }
    }

    /// # Safety
    /// None beyond the id being this process's.
    #[no_mangle]
    pub unsafe extern "C" fn mlc_side_drop(id: i64) {
        drop_entry(id);
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;

    pub(crate) static AFTER_TAKE_LOAD: Mutex<Option<fn()>> = Mutex::new(None);
    pub(super) static AFTER_APPEND_CHECK: Mutex<Option<fn()>> = Mutex::new(None);
    fn take_all(id: i64) -> Result<Vec<Vec<u8>>, MorlocError> {
        let mut r = take(id)?;
        let mut out = Vec::new();
        while let Some(rec) = r.next()? {
            out.push(rec);
        }
        Ok(out)
    }

    static HOOK_ID: AtomicI64 = AtomicI64::new(0);
    // 1: the commit writes nothing and fails; 2: it writes half the header and fails.
    pub(super) static FAIL_COMMIT: std::sync::atomic::AtomicU8 = std::sync::atomic::AtomicU8::new(0);

    // Runs a test's step once, at the point the hook names.
    pub(super) fn hook(at: &Mutex<Option<fn()>>) {
        let f = at.lock().unwrap_or_else(|p| p.into_inner()).take();
        if let Some(f) = f {
            f();
        }
    }

    #[test]
    fn a_fork_between_takes_read_and_its_end_loses_no_child_batch() {
        in_child(|| {
            let id = new(ptr::null_mut(), Kind::Gather).unwrap();
            HOOK_ID.store(id, Ordering::SeqCst);
            fn fork_a_writer() {
                let id = HOOK_ID.load(Ordering::SeqCst);
                let pid = unsafe { libc::fork() };
                if pid == 0 {
                    let ok = route(id).ok() == Some(false)
                        && lookup(id).is_some_and(|e| e.write_record(b"child").is_ok());
                    unsafe { libc::_exit(if ok { 0 } else { 1 }) };
                }
                let mut st = 0;
                unsafe { libc::waitpid(pid, &mut st, 0) };
                assert!(libc::WIFEXITED(st) && libc::WEXITSTATUS(st) == 0);
            }
            *AFTER_TAKE_LOAD.lock().unwrap() = Some(fork_a_writer);
            take_all(id).unwrap() == vec![b"child".to_vec()]
        });
    }

    #[test]
    fn an_owner_write_racing_take_is_in_the_result_or_fails() {
        in_child(|| {
            let id = new(ptr::null_mut(), Kind::Gather).unwrap();
            force_side(id);
            HOOK_ID.store(id, Ordering::SeqCst);
            fn take_meanwhile() {
                let id = HOOK_ID.load(Ordering::SeqCst);
                let t = std::thread::spawn(move || take_all(id));
                std::thread::sleep(std::time::Duration::from_millis(100));
                TAKER.lock().unwrap().replace(t);
            }
            *AFTER_APPEND_CHECK.lock().unwrap() = Some(take_meanwhile);
            let wrote = lookup(id).unwrap().write_record(b"late").is_ok();
            let taken = TAKER.lock().unwrap().take().unwrap().join().unwrap().unwrap();
            wrote == (taken == vec![b"late".to_vec()]) && (wrote || taken.is_empty())
        });
    }

    #[allow(clippy::type_complexity)]
    static TAKER: Mutex<Option<std::thread::JoinHandle<Result<Vec<Vec<u8>>, MorlocError>>>> = Mutex::new(None);

    // Every fork in the test binary switches every live entry (SIDE-9), so
    // each test runs alone in a forked child.
    fn in_child(f: fn() -> bool) {
        let _shm = crate::own_test_registry();
        let pid = unsafe { libc::fork() };
        assert!(pid >= 0);
        if pid == 0 {
            let ok = std::panic::catch_unwind(f).unwrap_or(false);
            unsafe { libc::_exit(if ok { 0 } else { 1 }) };
        }
        let mut st = 0;
        unsafe { libc::waitpid(pid, &mut st, 0) };
        assert!(libc::WIFEXITED(st) && libc::WEXITSTATUS(st) == 0, "status {st}");
    }

    pub(crate) fn side_with_record(id: i64) -> bool {
        force_side(id);
        write_side(id, b"side")
    }

    pub(crate) fn write_side(id: i64, rec: &[u8]) -> bool {
        lookup(id).is_some_and(|e| e.write_record(rec).is_ok())
    }

    fn state(id: i64) -> i64 {
        lookup(id).unwrap().state.load(Ordering::Acquire)
    }

    fn force_side(id: i64) {
        let e = lookup(id).unwrap();
        e.create_side().unwrap();
        e.state.store(SIDE, Ordering::Release);
    }

    fn reaped(pid: libc::pid_t) -> bool {
        let mut st = 0;
        unsafe { libc::waitpid(pid, &mut st, 0) };
        libc::WIFEXITED(st) && libc::WEXITSTATUS(st) == 0
    }

    #[test]
    fn a_put_after_take_fails_and_take_without_fork_has_no_side() {
        in_child(|| {
            let id = new(ptr::null_mut(), Kind::Gather).unwrap();
            route(id).unwrap() && take_all(id).unwrap().is_empty() && route(id).is_err() && take_all(id).is_err()
        });
    }

    #[test]
    fn an_unordered_owner_puts_natively_until_take() {
        in_child(|| {
            let id = new(ptr::null_mut(), Kind::Fold).unwrap();
            force_side(id);
            let native = entry(id, "fold").and_then(|e| route_entry(&e)).ok() == Some(true);
            let none = finish(id, Kind::Fold).1.is_ok_and(|mut r| r.next().unwrap().is_none());
            native && none && with_native(id, Kind::Fold, |_| ()).is_err()
        });
    }

    #[test]
    fn a_fold_owners_side_write_is_refused_and_writes_nothing() {
        in_child(|| {
            let id = new(ptr::null_mut(), Kind::Fold).unwrap();
            force_side(id);
            let e = lookup(id).unwrap();
            let refused = unsafe { put_side(id, None, ptr::null_mut()) }
                .err()
                .is_some_and(|e| format!("{e}").contains("native put was sent"));
            refused && committed(&e) == HEADER && finish(id, Kind::Fold).1.is_ok_and(|mut r| r.next().unwrap().is_none())
        });
    }

    #[test]
    fn a_call_of_the_other_kind_is_refused_and_leaves_the_entry() {
        in_child(|| {
            let mut native = 7u8;
            let p = &mut native as *mut u8 as *mut c_void;
            let gather = new(p, Kind::Gather).unwrap();
            let fold = new(p, Kind::Fold).unwrap();
            let refused = |r: Result<Option<()>, MorlocError>| {
                r.err().is_some_and(|e| format!("{e}").contains("internal error"))
            };
            let (fp, fr) = finish(gather, Kind::Fold);
            let (gp, gr) = finish(fold, Kind::Gather);
            let calls = refused(with_native(gather, Kind::Fold, |_| ()))
                && refused(with_native(fold, Kind::Gather, |_| ()))
                && route(fold).is_err()
                && fp.is_null()
                && fr.is_err()
                && gp.is_null()
                && gr.is_err();
            let drops = std::panic::catch_unwind(|| drop_native(gather, Kind::Fold)).is_err()
                && std::panic::catch_unwind(|| drop_native(fold, Kind::Gather)).is_err();
            let kept = with_native(gather, Kind::Gather, |n| n == p).unwrap() == Some(true)
                && with_native(fold, Kind::Fold, |n| n == p).unwrap() == Some(true);
            calls && drops && kept && finish(gather, Kind::Gather).0 == p && finish(fold, Kind::Fold).0 == p
        });
    }

    #[test]
    fn a_gather_owner_writes_the_side_once_switched() {
        in_child(|| {
            let id = new(ptr::null_mut(), Kind::Gather).unwrap();
            lookup(id).unwrap().state.store(SWITCHING, Ordering::Release);
            let switching = route(id).unwrap();
            lookup(id).unwrap().state.store(NATIVE, Ordering::Release);
            force_side(id);
            let side = !route(id).unwrap();
            lookup(id).unwrap().state.store(FAILED, Ordering::Release);
            switching && side && route(id).is_err() && take_all(id).is_err()
        });
    }

    #[test]
    fn a_forked_child_cannot_take_and_its_record_reaches_the_owner() {
        in_child(|| {
            let id = new(ptr::null_mut(), Kind::Gather).unwrap();
            force_side(id);
            let e = lookup(id).unwrap();
            e.write_record(b"parent").unwrap();
            let pid = unsafe { libc::fork() };
            if pid == 0 {
                let ok = route(id).ok() == Some(false) && take_all(id).is_err() && e.write_record(b"child").is_ok();
                unsafe { libc::_exit(if ok { 0 } else { 1 }) };
            }
            let child_ok = reaped(pid);
            let path = e.path().unwrap().to_string();
            let recs = take_all(id).unwrap();
            child_ok
                && recs == vec![b"parent".to_vec(), b"child".to_vec()]
                && !std::path::Path::new(&path).exists()
        });
    }

    #[test]
    fn a_write_after_take_fails() {
        in_child(|| {
            let id = new(ptr::null_mut(), Kind::Gather).unwrap();
            force_side(id);
            let e = lookup(id).unwrap();
            let path = e.path().unwrap().to_string();
            let records = take_all(id).unwrap();
            let gone = !std::path::Path::new(&path).exists();
            records.is_empty() && gone && e.write_record(b"late").is_err()
        });
    }

    fn committed(e: &SideEntry) -> u64 {
        read_header(e.lock_file().unwrap().0).unwrap().1
    }

    #[test]
    fn a_schemaless_child_batch_fails_the_take() {
        in_child(|| {
            let id = new(ptr::null_mut(), Kind::Gather).unwrap();
            let owner_refused = unsafe { put_side(id, None, ptr::null_mut()) }.is_err();
            force_side(id);
            let e = lookup(id).unwrap();
            let unwritten = committed(&e) == HEADER;
            let pid = unsafe { libc::fork() };
            if pid == 0 {
                let failed = unsafe { put_side(id, None, ptr::null_mut()) }.is_err();
                unsafe { libc::_exit(if failed { 0 } else { 1 }) };
            }
            let child_ok = reaped(pid);
            let failed = take_all(id).err().is_some_and(|e| format!("{e}").contains("no wire form"));
            owner_refused && unwritten && child_ok && failed
        });
    }

    #[test]
    fn a_marked_side_file_reports_its_poison_record() {
        in_child(|| {
            let id = new(ptr::null_mut(), Kind::Gather).unwrap();
            force_side(id);
            let pid = unsafe { libc::fork() };
            if pid == 0 {
                let failed = unsafe { put_side(id, None, ptr::null_mut()) }.is_err();
                fail(id);
                unsafe { libc::_exit(if failed { 0 } else { 1 }) };
            }
            let child_ok = reaped(pid);
            let named = take_all(id).err().is_some_and(|e| format!("{e}").contains("no wire form"));
            child_ok && named
        });
    }

    // tla:ForkSide_failed_put_unmarked.bug, tla:ForkSide_fold_failed_put_unmarked.bug
    #[test]
    fn a_failed_put_in_the_owner_fails_the_take() {
        in_child(|| {
            let g = new(ptr::null_mut(), Kind::Gather).unwrap();
            fail(g);
            let gather = take_all(g).err().is_some_and(|e| format!("{e}").contains("raised"));
            let f = new(ptr::null_mut(), Kind::Fold).unwrap();
            force_side(f);
            let path = lookup(f).unwrap().path().unwrap().to_string();
            fail(f);
            let fold = finish(f, Kind::Fold).1.err().is_some_and(|e| format!("{e}").contains("raised"));
            gather && fold && !std::path::Path::new(&path).exists()
        });
    }

    #[test]
    fn a_failed_put_after_take_changes_nothing() {
        in_child(|| {
            let id = new(ptr::null_mut(), Kind::Gather).unwrap();
            let taken = take_all(id).is_ok();
            fail(id);
            fail(-12345);
            taken
        });
    }

    #[test]
    fn a_failed_put_in_a_forked_child_fails_the_take_even_on_a_full_disk() {
        in_child(|| {
            let id = new(ptr::null_mut(), Kind::Gather).unwrap();
            force_side(id);
            let e = lookup(id).unwrap();
            e.write_record(b"parent").unwrap();
            let pid = unsafe { libc::fork() };
            if pid == 0 {
                let ok = write_capped(&e, &[7u8; 1000]) && {
                    let size = std::fs::metadata(e.path().unwrap()).unwrap().len();
                    let mut lim = libc::rlimit { rlim_cur: 0, rlim_max: 0 };
                    unsafe {
                        libc::getrlimit(libc::RLIMIT_FSIZE, &mut lim);
                        let capped = libc::rlimit { rlim_cur: size, rlim_max: lim.rlim_max };
                        libc::setrlimit(libc::RLIMIT_FSIZE, &capped);
                    }
                    fail(id);
                    e.write_record(b"later").is_err()
                };
                unsafe { libc::_exit(if ok { 0 } else { 1 }) };
            }
            let child_ok = reaped(pid);
            let path = e.path().unwrap().to_string();
            let failed = take_all(id).err().is_some_and(|e| format!("{e}").contains("forked child raised"));
            child_ok && failed && !std::path::Path::new(&path).exists()
        });
    }

    #[test]
    fn gather_ids_are_program_unique_and_a_foreign_one_is_refused() {
        in_child(|| {
            let flags = libc::MAP_SHARED | libc::MAP_ANONYMOUS;
            let page = unsafe { libc::mmap(ptr::null_mut(), 8, libc::PROT_READ | libc::PROT_WRITE, flags, -1, 0) };
            assert!(page != libc::MAP_FAILED);
            let slot = page as *mut i64;
            let pid = unsafe { libc::fork() };
            if pid == 0 {
                unsafe {
                    *slot = new(ptr::null_mut(), Kind::Gather).unwrap();
                    libc::_exit(0)
                };
            }
            let mine = new(ptr::null_mut(), Kind::Gather).unwrap();
            let child_ok = reaped(pid);
            let theirs = unsafe { *slot };
            unsafe { libc::munmap(page, 8) };
            let foreign = |r: Result<bool, MorlocError>| r.err().is_some_and(|e| format!("{e}").contains("another process"));
            let refused = foreign(route(theirs))
                && foreign(with_native(theirs, Kind::Gather, |_| ()).map(|r| r.is_some()));
            let taken = take_all(mine).is_ok();
            let late_mine = route(mine).err().is_some_and(|e| format!("{e}").contains("after its gather finished"));
            child_ok && mine != theirs && refused && taken && late_mine
        });
    }

    #[test]
    fn owner_threads_writing_while_take_runs_lose_and_tear_nothing() {
        in_child(|| {
            let id = new(ptr::null_mut(), Kind::Gather).unwrap();
            force_side(id);
            let e = lookup(id).unwrap();
            let (ok, taken) = std::thread::scope(|s| {
                let writers: Vec<_> = (0..2u8)
                    .map(|t| {
                        let e = e.clone();
                        s.spawn(move || {
                            (0..200u32)
                                .filter_map(|i| {
                                    let rec = [vec![t; 3000], i.to_le_bytes().to_vec()].concat();
                                    e.write_record(&rec).is_ok().then_some(rec)
                                })
                                .collect::<Vec<_>>()
                        })
                    })
                    .collect();
                std::thread::sleep(std::time::Duration::from_millis(2));
                let taken = take_all(id).unwrap();
                let ok: Vec<Vec<u8>> = writers.into_iter().flat_map(|w| w.join().unwrap()).collect();
                (ok, taken)
            });
            let mut a = ok.clone();
            let mut b = taken.clone();
            a.sort();
            b.sort();
            a == b && e.write_record(b"after").is_err()
        });
    }

    // Caps the file size so the next record's write fails midway.
    fn write_capped(e: &SideEntry, rec: &[u8]) -> bool {
        let size = std::fs::metadata(e.path().unwrap()).unwrap().len();
        let mut lim = libc::rlimit { rlim_cur: 0, rlim_max: 0 };
        unsafe {
            libc::signal(libc::SIGXFSZ, libc::SIG_IGN);
            libc::getrlimit(libc::RLIMIT_FSIZE, &mut lim);
            let capped = libc::rlimit { rlim_cur: size + 20, rlim_max: lim.rlim_max };
            libc::setrlimit(libc::RLIMIT_FSIZE, &capped);
        }
        let failed = e.write_record(rec).is_err();
        unsafe { libc::setrlimit(libc::RLIMIT_FSIZE, &lim) };
        failed
    }

    #[test]
    fn a_write_that_fails_midway_leaves_no_torn_record() {
        in_child(|| {
            let id = new(ptr::null_mut(), Kind::Gather).unwrap();
            force_side(id);
            let e = lookup(id).unwrap();
            e.write_record(b"first").unwrap();
            let failed = write_capped(&e, &[7u8; 1000]);
            e.write_record(b"last").unwrap();
            failed && take_all(id).unwrap() == vec![b"first".to_vec(), b"last".to_vec()]
        });
    }

    #[test]
    fn a_failed_commit_leaves_exactly_the_committed_records() {
        in_child(|| {
            let id = new(ptr::null_mut(), Kind::Gather).unwrap();
            force_side(id);
            let e = lookup(id).unwrap();
            e.write_record(b"first").unwrap();
            FAIL_COMMIT.store(1, Ordering::SeqCst);
            let failed = e.write_record(b"uncommitted").is_err();
            FAIL_COMMIT.store(0, Ordering::SeqCst);
            e.write_record(b"last").unwrap();
            failed && take_all(id).unwrap() == vec![b"first".to_vec(), b"last".to_vec()]
        });
    }

    #[test]
    fn a_torn_commit_fails_later_writes_and_the_take() {
        in_child(|| {
            let id = new(ptr::null_mut(), Kind::Gather).unwrap();
            force_side(id);
            let e = lookup(id).unwrap();
            e.write_record(b"first").unwrap();
            FAIL_COMMIT.store(2, Ordering::SeqCst);
            let failed = e.write_record(b"torn").is_err();
            FAIL_COMMIT.store(0, Ordering::SeqCst);
            failed && e.write_record(b"last").is_err() && take_all(id).is_err()
        });
    }

    fn reader_over(bytes: &[u8]) -> Reader {
        let path = std::env::temp_dir().join(format!("morloc-fork-side-test-{}", std::process::id()));
        std::fs::write(&path, bytes).unwrap();
        let c = CString::new(path.to_str().unwrap()).unwrap();
        let fd = unsafe { libc::open(c.as_ptr(), libc::O_RDONLY | libc::O_CLOEXEC) };
        std::fs::remove_file(&path).unwrap();
        assert!(fd >= 0);
        Reader { file: Some(SideFd(fd)), at: 1, end: bytes.len() as u64 }
    }

    #[test]
    fn a_truncated_record_fails_the_take() {
        let mut whole = vec![CLOSED];
        whole.extend_from_slice(&2u64.to_le_bytes());
        whole.extend_from_slice(b"ab");
        let mut short_body = whole.clone();
        short_body.extend_from_slice(&5u64.to_le_bytes());
        short_body.extend_from_slice(b"ab");
        let mut short_head = whole.clone();
        short_head.extend_from_slice(&[1, 0, 0]);
        let mut r = reader_over(&whole);
        assert_eq!(r.next().unwrap(), Some(b"ab".to_vec()));
        assert_eq!(r.next().unwrap(), None);
        for torn in [short_body, short_head] {
            let mut r = reader_over(&torn);
            assert_eq!(r.next().unwrap(), Some(b"ab".to_vec()));
            assert!(r.next().is_err());
        }
    }

    #[test]
    fn a_fork_settles_a_live_native_entry_and_a_spawn_does_not() {
        in_child(|| {
            let id = new(ptr::null_mut(), Kind::Gather).unwrap();
            let path = std::ffi::CString::new("/bin/sh").unwrap();
            let flag = std::ffi::CString::new("-c").unwrap();
            let script = std::ffi::CString::new("exit 0").unwrap();
            let argv = [path.as_ptr(), flag.as_ptr(), script.as_ptr(), ptr::null()];
            let mut child: libc::pid_t = 0;
            let rc = unsafe {
                libc::posix_spawn(&mut child, path.as_ptr(), ptr::null(), ptr::null(), argv.as_ptr() as *const *mut c_char, ptr::null())
            };
            let spawned = rc == 0 && reaped(child) && state(id) == NATIVE;
            let pid = unsafe { libc::fork() };
            if pid == 0 {
                unsafe { libc::_exit(0) };
            }
            let forked = reaped(pid);
            let settled = state(id);
            let _ = take_all(id);
            spawned && forked && settled == SIDE && FORKING.load(Ordering::SeqCst) == 0
        });
    }
}
