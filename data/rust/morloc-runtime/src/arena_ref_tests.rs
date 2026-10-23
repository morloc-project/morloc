//! SHM-14 against the runtime's eval arena: the actions of tla:ArenaRefs,
//! on real blocks. Every block also carries a reference the test holds, so
//! no block is freed or reused while a run inspects it.

use crate::eval_arena::{self, ArenaCheckpoint, ArenaGuard};
use crate::shm::{self, AbsPtr};

#[derive(Clone, Copy, PartialEq, Debug)]
enum Kind {
    Arena,
    Own,
    Acq,
    Struct,
}

#[derive(Clone, Copy, PartialEq, Debug)]
enum Phase {
    Before,
    Body,
    After,
    Ended,
}

#[derive(Debug)]
struct Ref {
    block: usize,
    kind: Kind,
    post: bool,
    live: bool,
}

struct Run {
    blocks: Vec<AbsPtr>,
    refs: Vec<Ref>,
    phase: Phase,
    cp: Option<ArenaCheckpoint>,
    guard: Option<ArenaGuard>,
    trace: Vec<String>,
    failure: Option<String>,
}

fn count(p: AbsPtr) -> u32 {
    unsafe { shm::reference_count(p) }.expect("a block the test holds")
}

impl Run {
    fn new() -> Run {
        Run {
            blocks: Vec::new(),
            refs: Vec::new(),
            phase: Phase::Before,
            cp: None,
            guard: Some(eval_arena::enter().unwrap()),
            trace: Vec::new(),
            failure: None,
        }
    }

    fn live(&self, k: Kind) -> Vec<usize> {
        (0..self.refs.len()).filter(|&r| self.refs[r].live && self.refs[r].kind == k).collect()
    }

    fn holders(&self, b: usize) -> u32 {
        self.refs.iter().filter(|r| r.live && r.block == b && r.kind != Kind::Arena).count() as u32
    }

    fn push(&mut self, block: usize, kind: Kind) -> usize {
        self.refs.push(Ref { block, kind, post: self.phase == Phase::Body, live: true });
        self.refs.len() - 1
    }

    fn alloc(&mut self, kind: Kind) -> usize {
        assert!(matches!(kind, Kind::Arena | Kind::Own));
        let p = shm::shmalloc(16).unwrap();
        unsafe { shm::shincref(p) }.unwrap();
        self.blocks.push(p);
        let r = self.push(self.blocks.len() - 1, kind);
        self.step(format!("alloc {kind:?} -> block {}, ref {r}", self.blocks.len() - 1));
        r
    }

    fn acquire(&mut self, b: usize) -> usize {
        unsafe { shm::shincref(self.blocks[b]) }.unwrap();
        let r = self.push(b, Kind::Acq);
        self.step(format!("acquire block {b} -> ref {r}"));
        r
    }

    fn own_free(&mut self, r: usize) {
        assert_eq!(self.refs[r].kind, Kind::Own);
        shm::shfree(self.blocks[self.refs[r].block]).unwrap();
        self.refs[r].live = false;
        self.step(format!("owner frees ref {r}"));
    }

    fn acq_release(&mut self, r: usize) {
        assert_eq!(self.refs[r].kind, Kind::Acq);
        shm::release(self.blocks[self.refs[r].block]).unwrap();
        self.refs[r].live = false;
        self.step(format!("acquirer releases ref {r}"));
    }

    fn escape(&mut self, r: usize) {
        assert_eq!(self.refs[r].kind, Kind::Own);
        self.refs[r].kind = Kind::Arena;
        self.step(format!("ref {r} left to the arena"));
    }

    fn hand_off(&mut self, r: usize) {
        assert!(matches!(self.refs[r].kind, Kind::Arena | Kind::Own));
        eval_arena::forget_if_active(self.blocks[self.refs[r].block]);
        self.refs[r].kind = Kind::Struct;
        self.step(format!("ref {r} handed to a structure"));
    }

    fn can_adopt(&self, r: usize) -> bool {
        let b = self.refs[r].block;
        self.refs[r].live
            && self.refs[r].kind == Kind::Struct
            && self.refs.iter().enumerate().all(|(i, x)| i == r || !x.live || x.block != b)
    }

    fn adopt(&mut self, r: usize) {
        assert!(self.can_adopt(r));
        eval_arena::record_if_active(self.blocks[self.refs[r].block]);
        self.refs[r].kind = Kind::Own;
        self.refs[r].post = self.phase == Phase::Body;
        self.step(format!("structure gives ref {r} to the arena"));
    }

    fn struct_release(&mut self, r: usize) {
        assert_eq!(self.refs[r].kind, Kind::Struct);
        shm::release(self.blocks[self.refs[r].block]).unwrap();
        self.refs[r].live = false;
        self.step(format!("structure releases ref {r}"));
    }

    fn checkpoint(&mut self) {
        assert_eq!(self.phase, Phase::Before);
        self.cp = Some(eval_arena::checkpoint());
        self.phase = Phase::Body;
        self.step("checkpoint".into());
    }

    fn handles_left(&self, post_only: bool) -> bool {
        self.refs.iter().any(|r| r.live && matches!(r.kind, Kind::Own | Kind::Acq) && (r.post || !post_only))
    }

    fn rollback(&mut self) {
        assert!(self.phase == Phase::Body && !self.handles_left(true));
        eval_arena::rollback_to(self.cp.take().unwrap());
        self.phase = Phase::After;
        for r in self.refs.iter_mut().filter(|r| r.kind == Kind::Arena && r.post) {
            r.live = false;
        }
        self.step("rollback".into());
        self.check_exact();
    }

    fn end(&mut self) {
        assert!(self.phase != Phase::Ended && !self.handles_left(false));
        drop(self.guard.take());
        self.phase = Phase::Ended;
        for r in self.refs.iter_mut().filter(|r| r.kind == Kind::Arena) {
            r.live = false;
        }
        self.step("arena ends".into());
        self.check_exact();
    }

    // SHM-14: after a rollback or the end, each block holds exactly the
    // test's reference and those of the live references.
    fn check_exact(&mut self) {
        if self.failure.is_some() {
            return;
        }
        for b in 0..self.blocks.len() {
            let want = 1 + self.refs.iter().filter(|r| r.live && r.block == b).count() as u32;
            let have = count(self.blocks[b]);
            if have != want {
                self.failure = Some(format!("block {b} has {have} references; {want} are held"));
                return;
            }
        }
    }

    // SHM-14: a holder's reference is never released by anyone else.
    fn step(&mut self, what: String) {
        self.trace.push(what);
        if self.failure.is_some() || self.phase == Phase::Ended {
            return;
        }
        for b in 0..self.blocks.len() {
            let need = 1 + self.holders(b);
            let have = count(self.blocks[b]);
            if have < need {
                self.failure = Some(format!(
                    "block {b} has {have} references; the test and its holders still hold {need}"
                ));
                return;
            }
        }
    }

    // After a failure the counts are no longer known, so nothing is
    // released: a block may already be another test's.
    fn finish(mut self) -> Result<(), String> {
        if self.phase != Phase::Ended && self.failure.is_none() {
            for r in 0..self.refs.len() {
                if self.refs[r].live && self.refs[r].kind == Kind::Own {
                    self.own_free(r);
                }
                if self.refs[r].live && self.refs[r].kind == Kind::Acq {
                    self.acq_release(r);
                }
            }
            self.end();
        }
        drop(self.guard.take());
        if self.failure.is_none() {
            for &p in &self.blocks {
                for _ in 0..count(p) {
                    shm::release(p).unwrap();
                }
            }
        }
        match self.failure {
            None => Ok(()),
            Some(f) => Err(format!("{f}\n  trace:\n    {}", self.trace.join("\n    "))),
        }
    }
}

struct Rng(u64);

impl Rng {
    fn below(&mut self, n: usize) -> usize {
        self.0 ^= self.0 << 13;
        self.0 ^= self.0 >> 7;
        self.0 ^= self.0 << 17;
        (self.0 % n as u64) as usize
    }

    fn pick(&mut self, v: &[usize]) -> Option<usize> {
        (!v.is_empty()).then(|| v[self.below(v.len())])
    }
}

const MAX_REFS: usize = 10;
const MAX_STEPS: usize = 30;

fn random_run(seed: u64) -> Result<(), String> {
    let mut rng = Rng(seed.wrapping_mul(0x9E37_79B9_7F4A_7C15) | 1);
    let mut run = Run::new();
    for _ in 0..MAX_STEPS {
        if run.failure.is_some() {
            break;
        }
        let room = run.refs.len() < MAX_REFS;
        match rng.below(12) {
            0 | 1 if room => {
                run.alloc(if rng.below(2) == 0 { Kind::Arena } else { Kind::Own });
            }
            2 if room && !run.blocks.is_empty() => {
                let b = rng.below(run.blocks.len());
                if run.refs.iter().any(|r| r.live && r.block == b) {
                    run.acquire(b);
                }
            }
            3 | 4 => {
                if let Some(r) = rng.pick(&run.live(Kind::Own)) {
                    run.own_free(r);
                }
            }
            5 => {
                if let Some(r) = rng.pick(&run.live(Kind::Acq)) {
                    run.acq_release(r);
                }
            }
            6 => {
                if let Some(r) = rng.pick(&run.live(Kind::Own)) {
                    run.escape(r);
                }
            }
            7 => {
                let mut h = run.live(Kind::Arena);
                h.extend(run.live(Kind::Own));
                if let Some(r) = rng.pick(&h) {
                    run.hand_off(r);
                }
            }
            8 => {
                let s: Vec<usize> = run.live(Kind::Struct).into_iter().filter(|&r| run.can_adopt(r)).collect();
                if let Some(r) = rng.pick(&s) {
                    run.adopt(r);
                }
            }
            9 => {
                if let Some(r) = rng.pick(&run.live(Kind::Struct)) {
                    run.struct_release(r);
                }
            }
            10 if run.phase == Phase::Before => run.checkpoint(),
            11 if run.phase == Phase::Body && !run.handles_left(true) => run.rollback(),
            _ => {}
        }
    }
    run.finish()
}

// SHM-14: tla:ArenaRefs's actions in random order never release a reference
// a holder keeps, and leave each block exactly what is still held.
#[test]
fn an_arena_conserves_the_references_it_records() {
    let _shm = crate::init_test_shm();
    for seed in 0..4000u64 {
        if let Err(e) = random_run(seed) {
            panic!("seed {seed}: {e}");
        }
    }
}

// SHM-14: a block recorded twice was released behind its first entry; the
// arena reports it and keeps one entry.
#[test]
fn recording_a_block_twice_is_refused() {
    let _shm = crate::init_test_shm();
    let seen = eval_arena::broken_rules_seen();
    let g = eval_arena::enter().unwrap();
    let p = shm::shmalloc(16).unwrap();
    unsafe { shm::shincref(p) }.unwrap();
    eval_arena::record_if_active(p);
    let entries = eval_arena::recorded_blocks();
    drop(g);
    let left = count(p);
    shm::release(p).unwrap();
    assert_eq!(eval_arena::broken_rules_seen(), seen + 1);
    assert_eq!(entries, 1);
    assert_eq!(left, 1, "the arena released a block more than once");
}

// SHM-14: giving the arena a block another holder keeps is reported.
#[test]
fn keeping_a_shared_block_is_reported() {
    let _shm = crate::init_test_shm();
    let seen = eval_arena::broken_rules_seen();
    let g = eval_arena::enter().unwrap();
    let p = shm::shmalloc(16).unwrap();
    assert!(eval_arena::keep_if_active(p));
    let alone = eval_arena::broken_rules_seen();
    unsafe { shm::shincref(p) }.unwrap();
    assert!(eval_arena::keep_if_active(p));
    drop(g);
    shm::release(p).unwrap();
    assert_eq!(alone, seen);
    assert_eq!(eval_arena::broken_rules_seen(), seen + 1);
}

// SHM-14: dropped entries are compacted without moving any across a
// checkpoint.
#[test]
fn a_rollback_after_compaction_releases_what_followed_its_checkpoint() {
    let _shm = crate::init_test_shm();
    let mut run = Run::new();
    let early: Vec<usize> = (0..6).map(|_| run.alloc(Kind::Own)).collect();
    run.checkpoint();
    let mut later = Vec::new();
    let mut freed = Vec::new();
    for i in 0..30 {
        if i % 2 == 0 {
            later.push(run.alloc(Kind::Arena));
        } else {
            freed.push(run.alloc(Kind::Own));
        }
    }
    let recorded = eval_arena::log_len();
    for r in early.iter().take(4).chain(freed.iter()) {
        run.own_free(*r);
    }
    let compacted = eval_arena::log_len() < recorded;
    for r in &early[4..] {
        run.escape(*r);
    }
    run.rollback();
    for r in &later {
        assert!(!run.refs[*r].live);
    }
    let kept = early[4..].iter().all(|&r| eval_arena::has_entry(run.blocks[run.refs[r].block]));
    run.end();
    run.finish().unwrap();
    assert!(compacted, "the frees never compacted the arena");
    assert!(kept, "the rollback released an entry recorded before its checkpoint");
}
