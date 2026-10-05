//! Fold accumulators for the `@fold` stream-handler form.
//!
//! A folding `@render`/`@with` handler turns a stream into one value
//! instead of a list. The producer still drives the loop and the sink is
//! still `[a] -> <IO> ()`, so nothing flows back out of it; the running
//! accumulator has to live behind a handle between batches. That handle is
//! a cell.
//!
//! ## Why a cell holds several values
//!
//! A producer may call its sink from several threads -- that is how a
//! native parallel map is written -- and the read-modify-write around a
//! morloc `step` cannot be made atomic from here, because applying `step`
//! means running user code in a pool. Serializing the whole update behind
//! one lock would make a threaded producer no faster than a sequential
//! one, and dropping the lock would lose updates outright.
//!
//! So a cell holds one slot per thread that touches it. Each thread folds
//! into its own slot with no contention, and the handler's `combine` is
//! applied to the slots at the end.
//!
//! ## What the three terms must satisfy
//!
//! Every slot starts from `init`, so `init` is folded in once per thread
//! that touches the cell. For the answer not to depend on how many threads
//! the producer happened to use, `init` must be an identity for `combine`:
//! `combine(init, x) == x`. That is the same contract every parallel fold
//! carries, and it is why the accumulator is a monoid rather than merely a
//! seed and a step.
//!
//! Slots are also merged in the order the threads first folded, which is
//! not reproducible between runs, so `combine` must be commutative as well
//! as associative for a threaded producer to give a stable answer.
//!
//! Neither law can be checked here: testing them means running user code,
//! and the runtime cannot apply a morloc function.
//!
//! Each fold pays two deep copies of the accumulator, so an accumulator
//! whose size grows with the stream makes the whole fold quadratic. The
//! form is for accumulators of bounded size.
//!
//! ## Ownership
//!
//! Every value the cell keeps is copied into a single self-contained SHM
//! block, and every value it hands back is a fresh single block. A pool
//! releases a value with one `shfree` of the root, so a value spread over
//! several blocks would leak all but the first; and a value the cell
//! merely borrowed would be freed underneath it when the pool released its
//! argument at end of dispatch.
//!
//! Blocks a cell keeps outlive the eval scope that allocated them, so they
//! are dropped from the per-eval arena (see [`crate::eval_arena`]).

use std::ffi::{c_char, c_void};
use std::ptr;
use std::thread::ThreadId;

use crate::cschema::CSchema;
use crate::error::MorlocError;
use crate::intrinsics::{current_temp_owner, wrap_c_call, TEMP_OWNER_NONE};
use crate::schema::Schema;
use crate::shm::{self, AbsPtr};
use morloc_runtime_types::shm_types::{self as shm_types_crate, RelPtr};
use crate::voidstar;

/// Slot index occupies the low 12 bits, a process tag the next 24, and
/// the generation the rest below the sign bit, so every valid handle is
/// positive and -1 is an unambiguous error sentinel.
///
/// The process tag is what makes a handle safe to reject rather than
/// misread. This registry is process-local, unlike the stream registry
/// which lives in a shared segment, so a handle that reaches another
/// worker process would otherwise resolve against whatever cell happens
/// to occupy that slot there -- and the first cell of two processes lands
/// at the same slot and generation, which is the case most likely to
/// arise. The tag is drawn per process rather than taken from the pid,
/// which is reused and bounded by `pid_max`.
const SLOT_BITS: u32 = 12;
const PROC_BITS: u32 = 24;
const SLOT_MASK: i64 = (1 << SLOT_BITS) - 1;
const PROC_MASK: i64 = (1 << PROC_BITS) - 1;
const MAX_CELLS: usize = 1 << SLOT_BITS;

/// An upper bound on the accumulators one cell may hold.
///
/// One per thread that folds, so a producer driving its sink from a
/// bounded worker pool stays far below this however long the stream is. A
/// producer that starts a fresh thread per batch does not: it would hold
/// an accumulator per batch, making the fold's memory grow with the stream
/// and its slot lookup quadratic -- the very costs the form exists to
/// remove. Refusing is better than quietly becoming worse than the gather.
const MAX_SLOTS: usize = 4096;

fn proc_tag() -> i64 {
    let pid = std::process::id() as u64;
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.subsec_nanos() as u64 ^ d.as_secs())
        .unwrap_or(0);
    // Any spread over the tag's range will do; this only has to make a
    // collision between two live pool processes unlikely.
    let mixed = pid
        .wrapping_mul(0x9E37_79B9_7F4A_7C15)
        .rotate_left(31)
        ^ now.wrapping_mul(0xBF58_476D_1CE4_E5B9);
    ((mixed ^ (mixed >> 29)) as i64) & PROC_MASK
}

static TAG: std::sync::atomic::AtomicI64 = std::sync::atomic::AtomicI64::new(-1);

fn tag() -> i64 {
    use std::sync::atomic::Ordering;
    let t = TAG.load(Ordering::Acquire);
    if t >= 0 {
        return t;
    }
    match TAG.compare_exchange(-1, proc_tag(), Ordering::AcqRel, Ordering::Acquire) {
        Ok(_) => TAG.load(Ordering::Acquire),
        Err(won) => won,
    }
}

// FORK-13: a child's tag never equals the tag of the parent it was forked from.
pub(crate) fn after_fork_in_child() {
    use std::sync::atomic::Ordering;
    let parent = TAG.load(Ordering::Relaxed);
    let mut t = proc_tag();
    if t == parent {
        t = (t + 1) & PROC_MASK;
    }
    TAG.store(t, Ordering::Release);
}

struct CellEntry {
    live: bool,
    /// Bumped on free, so a handle to a released cell is rejected rather
    /// than silently addressing whatever took its place.
    generation: i64,
    owner: u64,
    /// For an unowned cell, the next dispatch id when it was made (FORK-16).
    born: u64,
    /// The seed. Also the answer for a cell no thread ever folded into,
    /// which is what an empty stream must fold to.
    ///
    /// Held as a relptr, not an address: the registry is shared across
    /// threads and a raw pointer is not `Send`, and a relptr is what an
    /// SHM value stores anyway.
    init: RelPtr,
    slots: Vec<(ThreadId, RelPtr)>,
}

struct CellRegistry {
    cells: Vec<CellEntry>,
}

static CELL_REGISTRY: crate::fork_policy::Reset<CellRegistry> =
    crate::fork_policy::Reset::new(|| CellRegistry { cells: Vec::new() });

fn pack_handle(slot: usize, generation: i64) -> i64 {
    ((generation << (SLOT_BITS + PROC_BITS))
        | (tag() << SLOT_BITS)
        | (slot as i64))
        & i64::MAX
}

fn unpack_handle(h: i64) -> Option<(usize, i64)> {
    if h < 0 || (h >> SLOT_BITS) & PROC_MASK != tag() {
        return None;
    }
    Some(((h & SLOT_MASK) as usize, h >> (SLOT_BITS + PROC_BITS)))
}

/// Take a block out of the enclosing eval arena and record it as a
/// relptr: the cell's blocks span dispatches, so the arena must not
/// release them at scope exit.
fn cell_owns(p: AbsPtr) -> Result<RelPtr, MorlocError> {
    crate::eval_arena::forget_if_active(p);
    let rel = shm::abs2rel(p);
    if rel.is_err() {
        let _ = shm::shfree(p);
    }
    rel
}

fn free_block(rel: RelPtr) {
    if let Ok(p) = shm::rel2abs(rel) {
        let _ = shm::shfree(p);
    }
}


fn no_such_cell(fn_name: &str) -> MorlocError {
    MorlocError::Other(format!(
        "{}: no such fold accumulator; the handle is stale or was already released",
        fn_name
    ))
}

fn poisoned(fn_name: &str) -> MorlocError {
    MorlocError::Other(format!("{}: fold accumulator registry is poisoned", fn_name))
}

/// Resolve a handle to a live cell index, rejecting a stale generation.
fn resolve(reg: &CellRegistry, h: i64, fn_name: &str) -> Result<usize, MorlocError> {
    let (slot, gen) = unpack_handle(h).ok_or_else(|| no_such_cell(fn_name))?;
    match reg.cells.get(slot) {
        Some(c) if c.live && c.generation == gen => Ok(slot),
        _ => Err(no_such_cell(fn_name)),
    }
}

fn release_entry(c: &mut CellEntry) {
    free_block(c.init);
    for (_, p) in c.slots.drain(..) {
        free_block(p);
    }
    c.init = shm_types_crate::RELNULL;
    c.live = false;
    c.generation += 1;
}

// -- operations ---------------------------------------------------------
//
// Each takes a parsed schema so the tests can drive them without a
// CSchema; the `mlc_cell_*` entry points below are the C ABI over these.

/// Create a fold accumulator seeded with the value at `init`.
///
/// # Safety
/// `init` must point at a value laid out as `rs` describes.
pub unsafe fn cell_new(rs: &Schema, init: *const u8) -> Result<i64, MorlocError> {
    if init.is_null() {
        return Err(MorlocError::Other("mlc_cell_new: null init".into()));
    }
    let seed = cell_owns(voidstar::deep_copy_to_block(init, rs)?)?;
    let mut reg = match CELL_REGISTRY.lock() {
        Ok(r) => r,
        Err(_) => {
            free_block(seed);
            return Err(poisoned("mlc_cell_new"));
        }
    };
    let owner = current_temp_owner();
    if let Some(i) = reg.cells.iter().position(|c| !c.live) {
        let gen = reg.cells[i].generation;
        reg.cells[i] = CellEntry {
            live: true,
            generation: gen,
            owner,
            born: crate::intrinsics::next_dispatch_id(),
            init: seed,
            slots: Vec::new(),
        };
        return Ok(pack_handle(i, gen));
    }
    if reg.cells.len() >= MAX_CELLS {
        free_block(seed);
        return Err(MorlocError::Other(
            "mlc_cell_new: too many live fold accumulators".into(),
        ));
    }
    reg.cells.push(CellEntry {
        live: true,
        generation: 0,
        owner,
        born: crate::intrinsics::next_dispatch_id(),
        init: seed,
        slots: Vec::new(),
    });
    Ok(pack_handle(reg.cells.len() - 1, 0))
}

/// This thread's accumulator, as a fresh block the caller owns and frees.
/// A thread that has not folded yet reads the seed.
///
/// # Safety
/// `rs` must describe the type the cell was created with.
pub unsafe fn cell_get(handle: i64, rs: &Schema) -> Result<AbsPtr, MorlocError> {
    let me = std::thread::current().id();
    let reg = CELL_REGISTRY.lock().map_err(|_| poisoned("mlc_cell_get"))?;
    let i = resolve(&reg, handle, "mlc_cell_get")?;
    let c = &reg.cells[i];
    let src = c.slots.iter().find(|(t, _)| *t == me).map(|(_, p)| *p).unwrap_or(c.init);
    copy_out(reg, src, rs)
}

/// Copy a slot's value into a fresh block without holding the registry.
///
/// The copy is the expensive part, and holding the registry across it
/// would put every worker back in a queue behind one accumulator -- the
/// contention the per-thread slots exist to remove. Taking a reference to
/// the block first is what makes releasing the lock safe: the block
/// cannot be reclaimed underneath the copy even if the cell is freed.
///
/// Copied rather than lent because the caller releases what it is handed
/// and the slot must survive to be folded into again.
fn copy_out(
    reg: crate::fork_policy::ResetGuard<'_, CellRegistry>,
    src: RelPtr,
    rs: &Schema,
) -> Result<AbsPtr, MorlocError> {
    let abs = shm::rel2abs(src)?;
    // A failed acquire owns nothing, so there is nothing to release.
    // SAFETY: abs was just resolved from a live cell relptr.
    unsafe { shm::shincref(abs) }?;
    drop(reg);
    let out = unsafe { voidstar::deep_copy_to_block(abs, rs) };
    let _ = shm::shfree(abs);
    out
}

/// Replace this thread's accumulator.
///
/// # Safety
/// `value` must point at a value laid out as `rs` describes.
pub unsafe fn cell_put(handle: i64, rs: &Schema, value: *const u8) -> Result<(), MorlocError> {
    if value.is_null() {
        return Err(MorlocError::Other("mlc_cell_put: null value".into()));
    }
    let me = std::thread::current().id();
    // Copied before the lock is taken: a deep copy of a large
    // accumulator must not hold every other worker out of its own slot.
    let fresh = cell_owns(voidstar::deep_copy_to_block(value, rs)?)?;
    let mut reg = CELL_REGISTRY.lock().map_err(|_| poisoned("mlc_cell_put"))?;
    let i = match resolve(&reg, handle, "mlc_cell_put") {
        Ok(i) => i,
        Err(e) => {
            free_block(fresh);
            return Err(e);
        }
    };
    let c = &mut reg.cells[i];
    match c.slots.iter_mut().find(|(t, _)| *t == me) {
        Some(entry) => {
            free_block(entry.1);
            entry.1 = fresh;
        }
        None => {
            if c.slots.len() >= MAX_SLOTS {
                free_block(fresh);
                return Err(MorlocError::Other(format!(
                    "@fold: more than {} accumulators in one fold. A folding \
                     handler keeps one per thread that folds into it, so this \
                     producer is starting a thread per batch rather than \
                     driving its sink from a worker pool; the fold would use \
                     more memory than the gather it replaces.",
                    MAX_SLOTS
                )));
            }
            c.slots.push((me, fresh));
        }
    }
    Ok(())
}

/// How many accumulators the final merge must fold. Never zero: a cell no
/// thread touched answers with its seed, which is what an empty stream
/// folds to.
pub fn cell_count(handle: i64) -> Result<i64, MorlocError> {
    let reg = CELL_REGISTRY.lock().map_err(|_| poisoned("mlc_cell_count"))?;
    let i = resolve(&reg, handle, "mlc_cell_count")?;
    Ok(std::cmp::max(1, reg.cells[i].slots.len() as i64))
}

/// Accumulator `index`, as a fresh block the caller owns and frees.
///
/// # Safety
/// `rs` must describe the type the cell was created with.
pub unsafe fn cell_slot(handle: i64, index: i64, rs: &Schema) -> Result<AbsPtr, MorlocError> {
    let reg = CELL_REGISTRY.lock().map_err(|_| poisoned("mlc_cell_slot"))?;
    let i = resolve(&reg, handle, "mlc_cell_slot")?;
    let c = &reg.cells[i];
    let n = std::cmp::max(1, c.slots.len() as i64);
    if index < 0 || index >= n {
        return Err(MorlocError::Other(format!(
            "mlc_cell_slot: index {} out of range (cell holds {})",
            index, n
        )));
    }
    let src = c.slots.get(index as usize).map(|(_, p)| *p).unwrap_or(c.init);
    copy_out(reg, src, rs)
}

/// Release a cell and every accumulator in it.
pub fn cell_free(handle: i64) -> Result<(), MorlocError> {
    let mut reg = CELL_REGISTRY.lock().map_err(|_| poisoned("mlc_cell_free"))?;
    let i = resolve(&reg, handle, "mlc_cell_free")?;
    release_entry(&mut reg.cells[i]);
    Ok(())
}

// -- C ABI --------------------------------------------------------------

/// # Safety
/// `init` must point at a value laid out as `schema` describes.
#[no_mangle]
pub unsafe extern "C" fn mlc_cell_new(
    schema: *const CSchema,
    init: *const c_void,
    errmsg: *mut *mut c_char,
) -> i64 {
    wrap_c_call(errmsg, -1, || {
        let rs = require_schema(schema, "mlc_cell_new")?;
        cell_new(&rs, init as *const u8)
    })
}

/// # Safety
/// `schema` must describe the type the cell was created with.
#[no_mangle]
pub unsafe extern "C" fn mlc_cell_get(
    handle: i64,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut c_void {
    wrap_c_call(errmsg, ptr::null_mut(), || {
        let rs = require_schema(schema, "mlc_cell_get")?;
        cell_get(handle, &rs).map(|p| p as *mut c_void)
    })
}

/// # Safety
/// `value` must point at a value laid out as `schema` describes.
#[no_mangle]
pub unsafe extern "C" fn mlc_cell_put(
    handle: i64,
    schema: *const CSchema,
    value: *const c_void,
    errmsg: *mut *mut c_char,
) -> i32 {
    wrap_c_call(errmsg, 1, || {
        let rs = require_schema(schema, "mlc_cell_put")?;
        cell_put(handle, &rs, value as *const u8).map(|_| 0)
    })
}

#[no_mangle]
pub unsafe extern "C" fn mlc_cell_count(handle: i64, errmsg: *mut *mut c_char) -> i64 {
    wrap_c_call(errmsg, -1, || cell_count(handle))
}

/// # Safety
/// `schema` must describe the type the cell was created with.
#[no_mangle]
pub unsafe extern "C" fn mlc_cell_slot(
    handle: i64,
    index: i64,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut c_void {
    wrap_c_call(errmsg, ptr::null_mut(), || {
        let rs = require_schema(schema, "mlc_cell_slot")?;
        cell_slot(handle, index, &rs).map(|p| p as *mut c_void)
    })
}

#[no_mangle]
pub unsafe extern "C" fn mlc_cell_free(handle: i64, errmsg: *mut *mut c_char) -> i32 {
    wrap_c_call(errmsg, 1, || cell_free(handle).map(|_| 0))
}

unsafe fn require_schema(schema: *const CSchema, fn_name: &str) -> Result<Schema, MorlocError> {
    if schema.is_null() {
        return Err(MorlocError::Other(format!("{}: null schema", fn_name)));
    }
    Ok(CSchema::to_rust(schema))
}


// -- dispatch bracketing ------------------------------------------------

/// Release any cell still held by the dispatch that is ending, so a
/// handler that raised before its merge cannot leak one. Cells made on a
/// thread the runtime did not start carry no owner and are collected once
/// nothing is running, on the same reasoning as unowned temp files --
/// `last` is the temp registry's count of in-flight dispatches reaching
/// zero, which is the same count this registry would otherwise keep a
/// second copy of.
pub fn sweep_dispatch(call_id: u64, oldest: Option<u64>) {
    if let Ok(mut reg) = CELL_REGISTRY.lock() {
        for c in reg.cells.iter_mut() {
            if c.live
                && (c.owner == call_id
                    || (c.owner == TEMP_OWNER_NONE && crate::intrinsics::unowned_collectable(c.born, oldest)))
            {
                release_entry(c);
            }
        }
    }
}

#[cfg(test)]
pub fn live_cell_count() -> usize {
    CELL_REGISTRY.lock().map(|r| r.cells.iter().filter(|c| c.live).count()).unwrap_or(0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use morloc_runtime_types::schema::parse_schema;

    fn live_blocks() -> usize {
        let mut hist = [0usize; 40];
        shm::live_block_stats(&mut hist).0
    }

    /// Build a `[Str]` voidstar the way a pool would hand one over.
    unsafe fn mk(json: &str, schema: &Schema) -> AbsPtr {
        crate::json::read_json_with_schema(json, schema).unwrap()
    }

    /// A cell must neither leak nor double-free.
    ///
    /// Every block it keeps and every block it hands back is released
    /// exactly once, so a create/fold/merge/destroy cycle has to leave the
    /// live block count where it found it. The count, not the byte total,
    /// is the assertion: one stranded block per fold is the failure mode,
    /// whatever its size.
    #[test]
    fn a_fold_cycle_leaves_no_blocks_behind() {
        let _shm = crate::own_test_registry();
        let schema = parse_schema("as").unwrap();

        let cycle = || unsafe {
            let seed = mk("[]", &schema);
            let h = cell_new(&schema, seed).unwrap();
            shm::shfree(seed).unwrap();

            // Two batches folded into this thread's slot.
            for batch in ["[\"alpha\"]", "[\"alpha\",\"bravo\"]"] {
                let cur = cell_get(h, &schema).unwrap();
                shm::shfree(cur).unwrap();
                let next = mk(batch, &schema);
                cell_put(h, &schema, next).unwrap();
                shm::shfree(next).unwrap();
            }

            // The merge reads every slot, then releases the cell.
            let n = cell_count(h).unwrap();
            assert_eq!(n, 1);
            for i in 0..n {
                let v = cell_slot(h, i, &schema).unwrap();
                shm::shfree(v).unwrap();
            }
            cell_free(h).unwrap();
        };

        cycle();
        let before = live_blocks();
        for _ in 0..8 {
            cycle();
        }
        assert_eq!(live_blocks(), before, "fold cycle moved the live block count");
    }

    /// An untouched cell folds to its seed, so the merge always has
    /// something to reduce and an empty stream answers with `init`.
    #[test]
    fn an_untouched_cell_reads_back_its_seed() {
        let _shm = crate::own_test_registry();
        let schema = parse_schema("as").unwrap();
        unsafe {
            let seed = mk("[\"seed\"]", &schema);
            let h = cell_new(&schema, seed).unwrap();
            shm::shfree(seed).unwrap();

            assert_eq!(cell_count(h).unwrap(), 1);
            let v = cell_slot(h, 0, &schema).unwrap();
            let json = crate::json::voidstar_to_json_string(v, &schema).unwrap();
            assert_eq!(json, "[\"seed\"]");
            shm::shfree(v).unwrap();
            cell_free(h).unwrap();
        }
    }

    /// Each thread folds into its own slot, so a producer that calls the
    /// sink from several threads loses no updates and the merge sees one
    /// accumulator per thread.
    #[test]
    fn every_thread_that_folds_gets_its_own_slot() {
        let _shm = crate::own_test_registry();
        let schema = parse_schema("as").unwrap();
        unsafe {
            let seed = mk("[]", &schema);
            let h = cell_new(&schema, seed).unwrap();
            shm::shfree(seed).unwrap();

            let mut handles = Vec::new();
            for i in 0..4 {
                handles.push(std::thread::spawn(move || {
                    let schema = parse_schema("as").unwrap();
                    let v = mk(&format!("[\"t{}\"]", i), &schema);
                    cell_put(h, &schema, v).unwrap();
                    shm::shfree(v).unwrap();
                }));
            }
            for t in handles {
                t.join().unwrap();
            }

            assert_eq!(cell_count(h).unwrap(), 4);
            let mut seen: Vec<String> = Vec::new();
            for i in 0..4 {
                let v = cell_slot(h, i, &schema).unwrap();
                seen.push(crate::json::voidstar_to_json_string(v, &schema).unwrap());
                shm::shfree(v).unwrap();
            }
            seen.sort();
            assert_eq!(seen, vec!["[\"t0\"]", "[\"t1\"]", "[\"t2\"]", "[\"t3\"]"]);
            cell_free(h).unwrap();
        }
    }

    /// A released handle is rejected rather than addressing whatever cell
    /// took its place.
    #[test]
    fn a_released_handle_is_rejected() {
        let _shm = crate::own_test_registry();
        let schema = parse_schema("as").unwrap();
        unsafe {
            let seed = mk("[]", &schema);
            let h = cell_new(&schema, seed).unwrap();
            shm::shfree(seed).unwrap();
            cell_free(h).unwrap();

            assert!(cell_count(h).is_err());
            assert!(cell_get(h, &schema).is_err());
            assert!(cell_free(h).is_err());

            // The slot is reused, and the stale handle still does not reach
            // the cell that now occupies it.
            let seed2 = mk("[]", &schema);
            let h2 = cell_new(&schema, seed2).unwrap();
            shm::shfree(seed2).unwrap();
            assert_ne!(h, h2);
            assert!(cell_count(h).is_err());
            cell_free(h2).unwrap();
        }
    }

    /// A handle minted in another process names nothing here, and must be
    /// refused rather than resolved against whatever cell happens to sit
    /// at the same slot.
    #[test]
    fn a_handle_from_another_process_is_refused() {
        let _shm = crate::own_test_registry();
        let schema = parse_schema("as").unwrap();
        unsafe {
            let seed = mk("[]", &schema);
            let h = cell_new(&schema, seed).unwrap();
            shm::shfree(seed).unwrap();

            // Same slot and generation, a different process.
            let foreign = h ^ (1 << SLOT_BITS);
            assert_ne!(foreign, h);
            assert!(cell_count(foreign).is_err());
            assert!(cell_get(foreign, &schema).is_err());
            assert!(cell_free(foreign).is_err());

            assert!(cell_count(h).is_ok());
            cell_free(h).unwrap();
        }
    }

    /// A producer that starts a thread per batch would hold an accumulator
    /// per batch. That is refused rather than allowed to grow, because it
    /// would cost more than the gather the fold replaces.
    #[test]
    fn a_thread_per_batch_producer_is_refused() {
        let _shm = crate::own_test_registry();
        let schema = parse_schema("as").unwrap();
        unsafe {
            let seed = mk("[]", &schema);
            let h = cell_new(&schema, seed).unwrap();
            shm::shfree(seed).unwrap();

            // Rust never reuses a ThreadId, so each of these takes a slot.
            let mut refused = None;
            for i in 0..(MAX_SLOTS + 1) {
                let r = std::thread::spawn(move || {
                    let schema = parse_schema("as").unwrap();
                    let v = mk("[\"x\"]", &schema);
                    let r = cell_put(h, &schema, v);
                    shm::shfree(v).unwrap();
                    r
                })
                .join()
                .unwrap();
                if r.is_err() {
                    refused = Some(i);
                    break;
                }
            }
            assert_eq!(refused, Some(MAX_SLOTS), "cap fired at the wrong slot");
            assert_eq!(cell_count(h).unwrap(), MAX_SLOTS as i64);
            cell_free(h).unwrap();
        }
    }

    #[test]
    fn a_forked_child_never_resolves_its_parents_cell() {
        let _shm = crate::own_test_registry();
        let schema = parse_schema("as").unwrap();
        unsafe {
            let seed = mk("[\"x\"]", &schema);
            let h = cell_new(&schema, seed).unwrap();
            let ok = crate::fork_policy::exits_cleanly_in_a_forked_child(|| {
                let inherited = cell_count(h).is_err();
                let mine = cell_new(&schema, seed).unwrap();
                let distinct = mine != h && cell_count(h).is_err() && cell_count(mine).is_ok();
                cell_free(mine).unwrap();
                inherited && distinct
            });
            shm::shfree(seed).unwrap();
            assert!(cell_count(h).is_ok());
            cell_free(h).unwrap();
            assert!(ok, "a forked child resolved a fold accumulator its parent owns");
        }
    }

    /// A handler that raises before its merge leaves a live cell; the
    /// end-of-dispatch sweep reclaims it.
    #[test]
    fn an_abandoned_cell_is_swept_at_end_of_dispatch() {
        let _shm = crate::own_test_registry();
        let schema = parse_schema("as").unwrap();
        unsafe {
            let before = live_cell_count();
            let (id, prev) = crate::intrinsics::begin_dispatch();
            let seed = mk("[\"x\"]", &schema);
            let h = cell_new(&schema, seed).unwrap();
            shm::shfree(seed).unwrap();
            assert_eq!(live_cell_count(), before + 1);

            crate::intrinsics::end_dispatch(id, prev);
            assert_eq!(live_cell_count(), before);
            assert!(cell_count(h).is_err());
        }
    }
}
