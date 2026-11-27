use super::{died_inside, refuse_inside_txn, RegistrySlot, RelPtr, SlotField};
use crate::error::MorlocError;

// SLOT-39: readable anywhere, written only through a `SlotGuard` of its slot.
#[repr(transparent)]
pub struct BlockField(std::sync::atomic::AtomicIsize);

impl BlockField {
    #[inline]
    pub fn get(&self) -> RelPtr {
        self.0.load(std::sync::atomic::Ordering::Relaxed)
    }
}

// SLOT-39
#[derive(Clone, Copy)]
pub(crate) enum Block {
    FilePath,
    SchemaStr,
    SubpacketEntries,
    WriteBuffer,
    Custody,
}

// SLOT-4: a panic while it is held poisons the slot, as a death would.
pub(crate) struct SlotGuard<'a> {
    slot: &'a RegistrySlot,
    _held: morloc_runtime_types::recoverable_lock::RecoverableGuard<'a>,
}

impl Drop for SlotGuard<'_> {
    fn drop(&mut self) {
        if std::thread::panicking() {
            self.slot.poisoned.set(1);
        }
    }
}

impl<'a> SlotGuard<'a> {
    // SLOT-4
    pub(crate) fn lock(slot: &'a RegistrySlot) -> Result<Self, MorlocError> {
        let guard = Self::lock_any(slot)?;
        if slot.poisoned.get() != 0 {
            return Err(died_inside());
        }
        Ok(guard)
    }

    // SLOT-4: taken whatever the slot's state, to end or reclaim it.
    pub(crate) fn lock_any(slot: &'a RegistrySlot) -> Result<Self, MorlocError> {
        refuse_inside_txn()?;
        let acquired = slot.lock.lock()?;
        if acquired.holder_died {
            slot.poisoned.set(1);
        }
        Ok(SlotGuard { slot, _held: acquired.guard })
    }

    #[inline]
    pub(crate) fn slot(&self) -> &'a RegistrySlot {
        self.slot
    }

    // SLOT-39
    #[inline]
    pub(crate) fn set_block(&self, field: Block, v: RelPtr) {
        let f = match field {
            Block::FilePath => &self.slot.file_path,
            Block::SchemaStr => &self.slot.schema_str,
            Block::SubpacketEntries => &self.slot.subpacket_entries,
            Block::WriteBuffer => &self.slot.write_buffer,
            Block::Custody => &self.slot.custody,
        };
        f.0.store(v, std::sync::atomic::Ordering::Relaxed);
    }
}
