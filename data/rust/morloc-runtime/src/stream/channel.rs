use super::{free_uncounted, shm_copy_bytes, slot_owns, AbsPtr, RelPtr, SlotGuard, Unqueued};
use crate::error::MorlocError;
use std::sync::atomic::AtomicU32;

const CHANNEL_RUNNING: u32 = 0;
const CHANNEL_DONE: u32 = 1;
const CHANNEL_FAILED: u32 = 2;

const RELNULL: RelPtr = morloc_runtime_types::shm_types::RELNULL;

#[repr(C)]
struct ChannelBlock {
    status: u32,
    delivered: u32,
    count: u64,
    head: RelPtr,
    tail: RelPtr,
    fail_msg: RelPtr,
    fail_len: u64,
    // SLOT-21
    seq: AtomicU32,
    // SLOT-21
    waiters: u32,
}

#[repr(C)]
struct ChannelNode {
    next: RelPtr,
    arr: RelPtr,
}

// SLOT-39: a fresh, running channel block, not yet published in a slot.
pub(super) fn new_block() -> Result<AbsPtr, MorlocError> {
    let block_abs = crate::shm::shcalloc(1, std::mem::size_of::<ChannelBlock>())?;
    let b = block_abs as *mut ChannelBlock;
    // SAFETY: SHM-6: a zeroed block of the right size, reachable from nowhere yet.
    unsafe {
        (*b).status = CHANNEL_RUNNING;
        (*b).head = RELNULL;
        (*b).tail = RELNULL;
        (*b).fail_msg = RELNULL;
    }
    Ok(block_abs)
}

// SLOT-39: the channel block of a slot, reached only through a guard of it.
pub(super) struct Channel<'g> {
    b: *mut ChannelBlock,
    _guard: std::marker::PhantomData<&'g ()>,
}

pub(super) enum Taken {
    Batch(AbsPtr),
    End,
    Empty,
}

impl<'a> SlotGuard<'a> {
    // SLOT-39
    pub(super) fn channel(&self) -> Result<Channel<'_>, MorlocError> {
        let b = crate::shm::rel2abs(self.slot().subpacket_entries.get())? as *mut ChannelBlock;
        Ok(Channel { b, _guard: std::marker::PhantomData })
    }
}

impl Channel<'_> {
    pub(super) fn count(&self) -> u64 {
        // SAFETY: SLOT-39
        unsafe { (*self.b).count }
    }

    // SLOT-21: the caller has just changed the channel.
    fn changed(&self) {
        // SAFETY: SLOT-21: the block is the slot's, held under its lock.
        let (seq, waiters) = unsafe { (&(*self.b).seq, (*self.b).waiters) };
        seq.fetch_add(1, std::sync::atomic::Ordering::Release);
        if waiters > 0 {
            morloc_runtime_types::wait_word::wake_all(seq);
        }
    }

    // SLOT-21: register as a waiter and read the word; the caller waits after
    // releasing the lock and deregisters at its next look.
    pub(super) fn seen(&self) -> (&'static AtomicU32, u32) {
        // SAFETY: SLOT-21: shared memory stays mapped (DAEMON-1), so the word
        // stays readable after the block is freed; a wait on it then ends at
        // the slice and the slot's generation answers.
        let seq: &'static AtomicU32 = unsafe {
            (*self.b).waiters += 1;
            &(*self.b).seq
        };
        let seen = seq.load(std::sync::atomic::Ordering::Acquire);
        (seq, seen)
    }

    // SLOT-21: end a registration made under the lock earlier.
    pub(super) fn unwait(&self, registered: &mut bool) {
        if std::mem::take(registered) {
            // SAFETY: SLOT-21: the slot still holds the channel this side registered on.
            unsafe { (*self.b).waiters = (*self.b).waiters.saturating_sub(1) };
        }
    }

    // SLOT-35: a channel that is not running takes no writes; a failed one
    // says why.
    pub(super) fn refusal(&self, finished: &str) -> Option<MorlocError> {
        // SAFETY: SLOT-39
        match unsafe { (*self.b).status } {
            CHANNEL_RUNNING => None,
            _ => Some(self.failure().unwrap_or_else(|| MorlocError::Other(finished.into()))),
        }
    }

    pub(super) fn failure(&self) -> Option<MorlocError> {
        // SAFETY: SLOT-39
        (unsafe { (*self.b).status } == CHANNEL_FAILED).then(|| MorlocError::Other(self.fail_message()))
    }

    pub(super) fn delivered_failure(&self) -> Option<String> {
        // SAFETY: SLOT-39
        let delivered = unsafe { (*self.b).status == CHANNEL_FAILED && (*self.b).delivered != 0 };
        delivered.then(|| self.fail_message())
    }

    fn fail_message(&self) -> String {
        // SAFETY: SLOT-39
        unsafe {
            if (*self.b).fail_msg == RELNULL {
                return "the stream's producer failed".into();
            }
            match crate::shm::rel2abs((*self.b).fail_msg) {
                Ok(p) => String::from_utf8_lossy(std::slice::from_raw_parts(p, (*self.b).fail_len as usize)).into_owned(),
                Err(_) => "the stream's producer failed".into(),
            }
        }
    }

    pub(super) fn push(&self, arr: AbsPtr) -> Result<(), MorlocError> {
        let b = self.b;
        let arr = Unqueued(arr);
        let node = Unqueued(crate::shm::shmalloc(std::mem::size_of::<ChannelNode>())?);
        // SAFETY: SLOT-4
        let tail = match unsafe { (*b).tail } {
            RELNULL => None,
            t => Some(crate::shm::rel2abs(t)? as *mut ChannelNode),
        };
        let node_rel = crate::shm::abs2rel(node.0)?;
        let arr_rel = crate::shm::abs2rel(arr.0)?;
        let node_abs = node.0 as *mut ChannelNode;
        std::mem::forget(node);
        std::mem::forget(arr);
        // SAFETY: SLOT-4
        unsafe {
            (*node_abs).next = RELNULL;
            (*node_abs).arr = slot_owns(arr_rel);
            match tail {
                None => (*b).head = node_rel,
                Some(tail) => (*tail).next = node_rel,
            }
            (*b).tail = slot_owns(node_rel);
            (*b).count += 1;
        }
        self.changed();
        Ok(())
    }

    // SLOT-35: a failure handed to a reader is marked delivered.
    pub(super) fn take(&self) -> Result<Taken, MorlocError> {
        let b = self.b;
        // SAFETY: SLOT-39
        unsafe {
            if (*b).count > 0 {
                let node_abs = crate::shm::rel2abs((*b).head)?;
                let node = node_abs as *mut ChannelNode;
                let arr = crate::shm::rel2abs((*node).arr)?;
                (*b).head = (*node).next;
                if (*b).head == RELNULL {
                    (*b).tail = RELNULL;
                }
                (*b).count -= 1;
                self.changed();
                free_uncounted(node_abs);
                // SHM-8: the batch is this process's from here.
                crate::shm::take_on_reference();
                return Ok(Taken::Batch(arr));
            }
            match (*b).status {
                CHANNEL_DONE => Ok(Taken::End),
                CHANNEL_FAILED => {
                    (*b).delivered = 1;
                    Err(MorlocError::Other(self.fail_message()))
                }
                _ => Ok(Taken::Empty),
            }
        }
    }

    pub(super) fn finish(&self) {
        // SAFETY: SLOT-39
        unsafe {
            if (*self.b).status == CHANNEL_RUNNING {
                (*self.b).status = CHANNEL_DONE;
                self.changed();
            }
        }
    }

    pub(super) fn fail(&self, msg: &str) {
        let b = self.b;
        // SAFETY: SLOT-39
        unsafe {
            if (*b).status != CHANNEL_RUNNING {
                return;
            }
            (*b).status = CHANNEL_FAILED;
            if let Ok(rel) = shm_copy_bytes(msg.as_bytes()) {
                (*b).fail_msg = slot_owns(rel);
                (*b).fail_len = msg.len() as u64;
            }
        }
        self.changed();
    }

    // SLOT-39: the block itself is freed with the slot's other blocks.
    pub(super) fn free_queue(&self) {
        let b = self.b;
        // SLOT-21: waiters look again and find the slot released.
        self.changed();
        // SAFETY: SLOT-39
        unsafe {
            let mut node_rel = (*b).head;
            while node_rel != RELNULL {
                let Ok(node_abs) = crate::shm::rel2abs(node_rel) else { break };
                let node = node_abs as *mut ChannelNode;
                if let Ok(arr) = crate::shm::rel2abs((*node).arr) {
                    free_uncounted(arr);
                }
                node_rel = (*node).next;
                free_uncounted(node_abs);
            }
            (*b).head = RELNULL;
            (*b).tail = RELNULL;
            (*b).count = 0;
            if (*b).fail_msg != RELNULL {
                if let Ok(p) = crate::shm::rel2abs((*b).fail_msg) {
                    free_uncounted(p);
                }
                (*b).fail_msg = RELNULL;
            }
        }
    }
}
