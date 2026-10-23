// Function values (HOST-8). A morloc function value in a Rust pool is a plain
// `Arc<dyn Fn(&A1, .., &An) -> R + Send + Sync>`, so host code holds and calls
// it as it would any Rust closure. A value that can cross pools also needs its
// origin (the manifold to call back into and its serialized captures), and a
// staged value needs the step morloc runs when it is applied to its first
// argument. A `dyn Fn` carries neither, so they live in an environment found
// through a registry keyed by the address of the value's allocation (STATE-4).
//
// The panic classifier passes over the frames of the closures built here
// ('applies_a_function_value', PANIC-16).

use std::any::TypeId;
use std::collections::HashMap;
use morloc_runtime_types::hash::AddrMap;
use std::hash::BuildHasherDefault;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, OnceLock, PoisonError, RwLock};

/// The reified origin of a crossing closure: (home socket name, home manifold
/// id, serialized captured environment).
pub type ClosureOrigin = (String, i64, Vec<Vec<u8>>);

/// Raise on a function value that has no origin to send. A closure morloc
/// built carries the manifold to call back into; a callable host code made
/// has no such identity, and neither does a closure whose context is not
/// wholly native (those cross by the serial path instead). Every site that
/// needs an origin goes through here, so the failure names its cause once.
pub fn require_origin(origin: Option<ClosureOrigin>) -> ClosureOrigin {
    match origin {
        Some(o) => o,
        None => crate::morloc_throw(
            "cannot send a function value that morloc did not create: it has no \
             manifold to call back into. Apply it in the pool that received it, \
             or have the host return the data it would compute."
                .to_string(),
        ),
    }
}

// ---- registry ----------------------------------------------------------------

#[derive(Clone, Copy)]
struct Entry {
    env: usize,
    reify: unsafe fn(usize) -> Option<ClosureOrigin>,
    // The staged step: its type, and where it is in the environment.
    step: Option<(TypeId, usize)>,
}

type Shard = RwLock<AddrMap<Entry>>;

const SHARDS: usize = 64;

static REGISTRY: [Shard; SHARDS] =
    [const { RwLock::new(HashMap::with_hasher(BuildHasherDefault::new())) }; SHARDS];

// Staged values alive: while there are none, applying a value to its first
// argument needs no lookup.
static STAGED_LIVE: AtomicUsize = AtomicUsize::new(0);

// The shard comes from bits the map's own hash does not use for its tags
// (the top bits of `morloc_runtime_types::hash::AddrHasher`).
fn shard(key: usize) -> &'static Shard {
    &REGISTRY[(((key as u64) >> 4).wrapping_mul(0xD6E8_FEB8_6659_FD93) >> 32) as usize & (SHARDS - 1)]
}

// STATE-4: a poisoned shard's map is still consistent.
fn lookup(key: usize) -> Option<Entry> {
    shard(key).read().unwrap_or_else(PoisonError::into_inner).get(&key).copied()
}

fn key_of<T: ?Sized>(f: &Arc<T>) -> usize {
    Arc::as_ptr(f) as *const () as usize
}

enum Mk<C> {
    Origin(fn(&C) -> ClosureOrigin),
    Maybe(fn(&C) -> Option<ClosureOrigin>),
    Fixed,
}

struct Env<C> {
    caps: C,
    mk: Mk<C>,
    origin: OnceLock<Option<ClosureOrigin>>,
    key: usize,
    staged: bool,
}

/// STATE-4: a registered value's environment.
pub struct Guard<C>(*mut Env<C>);

// SAFETY: STATE-4: the guard is the environment's only owner.
unsafe impl<C: Send + Sync> Send for Guard<C> {}
// SAFETY: STATE-4: the environment is only read through the guard.
unsafe impl<C: Send + Sync> Sync for Guard<C> {}

impl<C> Guard<C> {
    #[inline]
    pub fn caps(&self) -> &C {
        // SAFETY: STATE-4: the environment lives as long as the guard.
        unsafe { &(*self.0).caps }
    }
}

impl<C> Drop for Guard<C> {
    fn drop(&mut self) {
        // SAFETY: STATE-4: the guard owns the environment from `into_raw`.
        let env = unsafe { Box::from_raw(self.0) };
        let key = env.key;
        if key != 0 {
            // STATE-4: the entry goes before the allocation it names is freed.
            let removed = shard(key).write().unwrap_or_else(PoisonError::into_inner).remove(&key);
            debug_assert!(removed.is_some_and(|e| e.env == self.0 as usize));
        }
        if env.staged {
            STAGED_LIVE.fetch_sub(1, Ordering::Release);
        }
    }
}

unsafe fn reify_env<C>(p: usize) -> Option<ClosureOrigin> {
    // SAFETY: STATE-4: an entry names a live environment of type C.
    let env = unsafe { &*(p as *const Env<C>) };
    env.origin
        .get_or_init(|| match env.mk {
            Mk::Origin(mk) => Some(mk(&env.caps)),
            Mk::Maybe(mk) => mk(&env.caps),
            Mk::Fixed => None,
        })
        .clone()
}

fn registered<C: Send + Sync + 'static, T: ?Sized>(
    caps: C,
    mk: Mk<C>,
    origin: Option<ClosureOrigin>,
    step: fn(&C) -> Option<(TypeId, usize)>,
    make: impl FnOnce(Guard<C>) -> Arc<T>,
) -> Arc<T> {
    let env = Box::new(Env { caps, mk, origin: OnceLock::new(), key: 0, staged: false });
    if origin.is_some() {
        let _ = env.origin.set(origin);
    }
    let envp: *mut Env<C> = Box::into_raw(env);
    // SAFETY: STATE-4: `envp` is live until its guard drops.
    let step = step(unsafe { &(*envp).caps });
    if step.is_some() {
        // SAFETY: STATE-4: no guard or closure reads the environment yet.
        unsafe { (*envp).staged = true };
        STAGED_LIVE.fetch_add(1, Ordering::Release);
    }
    let f = make(Guard(envp));
    let key = key_of(&f);
    // SAFETY: STATE-4: `f` holds the guard, and `f` has not escaped yet.
    unsafe { (*envp).key = key };
    // STATE-4: registered before the value is returned.
    shard(key)
        .write()
        .unwrap_or_else(PoisonError::into_inner)
        .insert(key, Entry { env: envp as usize, reify: reify_env::<C>, step });
    f
}

fn no_step<C>(_: &C) -> Option<(TypeId, usize)> {
    None
}

/// The origin of a function value, if morloc built it able to cross.
pub fn reify<T: ?Sized>(f: &Arc<T>) -> Option<ClosureOrigin> {
    let e = lookup(key_of(f))?;
    // SAFETY: STATE-4: `f` keeps the entry's environment alive.
    unsafe { (e.reify)(e.env) }
}

/// A capture-free closure as a plain function pointer. Coercion happens at
/// this call, so a caller never has to name the result type.
pub fn fn_ptr0<R>(f: fn() -> R) -> fn() -> R {
    f
}

// ---- per arity -----------------------------------------------------------------

macro_rules! fv_arity {
    ($value:ident, $local:ident, $crossing:ident, $proxy:ident, $cb:ident, $invoke:ident, ($($A:ident $a:ident),*)) => {
        pub type $value<$($A,)* R> = Arc<dyn Fn($(&$A),*) -> R + Send + Sync>;

        /// A function value that cannot cross pools.
        pub fn $local<C: Send + Sync + 'static, $($A: 'static,)* R: 'static>(
            caps: C,
            call: fn(&C, $(&$A),*) -> R,
        ) -> $value<$($A,)* R> {
            Arc::new(move |$($a: &$A),*| call(&caps, $($a),*))
        }

        /// A function value that can cross pools: its origin is built from
        /// its captures on first use.
        pub fn $crossing<C: Send + Sync + 'static, $($A: 'static,)* R: 'static>(
            caps: C,
            call: fn(&C, $(&$A),*) -> R,
            mk: fn(&C) -> ClosureOrigin,
        ) -> $value<$($A,)* R> {
            registered(caps, Mk::Origin(mk), None, no_step::<C>, |g| {
                Arc::new(move |$($a: &$A),*| call(g.caps(), $($a),*)) as $value<$($A,)* R>
            })
        }

        /// A function value reflected from another pool. It answers with the
        /// origin it ARRIVED with, so a value crossing A -> B -> C calls back
        /// to A rather than to B.
        pub fn $proxy<C: Send + Sync + 'static, $($A: 'static,)* R: 'static>(
            caps: C,
            call: fn(&C, $(&$A),*) -> R,
            origin: ClosureOrigin,
        ) -> $value<$($A,)* R> {
            registered(caps, Mk::Fixed, Some(origin), no_step::<C>, |g| {
                Arc::new(move |$($a: &$A),*| call(g.caps(), $($a),*)) as $value<$($A,)* R>
            })
        }

        /// What a sourced function's callback parameter receives: a closure
        /// passes as it is; a function value as an owned closure calling it.
        pub trait $cb<$($A,)* R> {
            fn into_callback(self) -> impl Fn($(&$A),*) -> R + Send + Sync + 'static;
        }
        impl<$($A,)* R, F: Fn($(&$A),*) -> R + Send + Sync + 'static> $cb<$($A,)* R> for F {
            #[inline]
            fn into_callback(self) -> impl Fn($(&$A),*) -> R + Send + Sync + 'static {
                self
            }
        }
        impl<$($A: 'static,)* R: 'static> $cb<$($A,)* R> for $value<$($A,)* R> {
            #[inline]
            fn into_callback(self) -> impl Fn($(&$A),*) -> R + Send + Sync + 'static {
                move |$($a: &$A),*| self($($a),*)
            }
        }

        /// What the runtime's intrinsics take as a function: any closure, or
        /// a function value, held or borrowed.
        pub trait $invoke<$($A,)* R> {
            fn invoke(&self, $($a: &$A),*) -> R;
        }
        impl<$($A,)* R, F: Fn($(&$A),*) -> R> $invoke<$($A,)* R> for F {
            #[inline]
            fn invoke(&self, $($a: &$A),*) -> R {
                self($($a),*)
            }
        }
        impl<$($A,)* R> $invoke<$($A,)* R> for $value<$($A,)* R> {
            #[inline]
            fn invoke(&self, $($a: &$A),*) -> R {
                self($($a),*)
            }
        }
        impl<$($A,)* R> $invoke<$($A,)* R> for &$value<$($A,)* R> {
            #[inline]
            fn invoke(&self, $($a: &$A),*) -> R {
                self($($a),*)
            }
        }
    };
}

macro_rules! fv_apply {
    ($value:ident, $prev:ident, $step:ident, $pap:ident, $staged:ident, $apply1:ident, $fnptr:ident,
     ($H:ident $h:ident) $(, ($T:ident $t:ident))*) => {
        /// How a staged value applies itself to its first argument.
        pub type $step<$H, $($T,)* R> = Arc<dyn Fn(&$H) -> $prev<$($T,)* R> + Send + Sync>;

        /// A capture-free closure as a plain function pointer.
        pub fn $fnptr<$H, $($T,)* R>(f: fn(&$H, $(&$T),*) -> R) -> fn(&$H, $(&$T),*) -> R {
            f
        }

        /// A function value applied to its first argument, keeping the
        /// argument. If `wire` serializes it, the result can cross: its
        /// origin is the function's with the argument appended to its
        /// captured values.
        pub fn $pap<$H: Clone + Send + Sync + 'static, $($T: 'static,)* R: 'static>(
            f: &$value<$H, $($T,)* R>,
            x: &$H,
            wire: Option<fn(&$H) -> Vec<u8>>,
        ) -> $prev<$($T,)* R> {
            match wire {
                None => {
                    let (f, x) = (f.clone(), x.clone());
                    Arc::new(move |$($t: &$T),*| f(&x, $($t),*))
                }
                Some(w) => {
                    type Caps<$H, $($T,)* R> = ($value<$H, $($T,)* R>, $H, fn(&$H) -> Vec<u8>);
                    fn mk<$H, $($T,)* R>(c: &Caps<$H, $($T,)* R>) -> Option<ClosureOrigin> {
                        let (home, mid, mut caps) = reify(&c.0)?;
                        caps.push((c.2)(&c.1));
                        Some((home, mid, caps))
                    }
                    registered((f.clone(), x.clone(), w), Mk::Maybe(mk::<$H, $($T,)* R>), None, no_step, |g| {
                        Arc::new(move |$($t: &$T),*| (g.caps().0)(&g.caps().1, $($t),*)) as $prev<$($T,)* R>
                    })
                }
            }
        }

        /// A function value that knows how morloc applies it to its first
        /// argument: `step` does it (a staged function runs its stage when it
        /// has the arguments before the stage point). Called with every
        /// argument, it is `flat`, and it crosses as `flat` does.
        pub fn $staged<$H: 'static, $($T: 'static,)* R: 'static>(
            flat: $value<$H, $($T,)* R>,
            step: $step<$H, $($T,)* R>,
        ) -> $value<$H, $($T,)* R> {
            type Caps<$H, $($T,)* R> = ($value<$H, $($T,)* R>, $step<$H, $($T,)* R>);
            fn mk<$H, $($T,)* R>(c: &Caps<$H, $($T,)* R>) -> Option<ClosureOrigin> {
                reify(&c.0)
            }
            fn step_of<$H: 'static, $($T: 'static,)* R: 'static>(c: &Caps<$H, $($T,)* R>) -> Option<(TypeId, usize)> {
                Some((TypeId::of::<$step<$H, $($T,)* R>>(), &c.1 as *const $step<$H, $($T,)* R> as usize))
            }
            registered((flat, step), Mk::Maybe(mk::<$H, $($T,)* R>), None, step_of::<$H, $($T,)* R>, |g| {
                Arc::new(move |$h: &$H, $($t: &$T),*| (g.caps().0)($h, $($t),*)) as $value<$H, $($T,)* R>
            })
        }

        /// Apply a function value to its first argument the way morloc does:
        /// a staged value runs its step; any other keeps the argument.
        pub fn $apply1<$H: Clone + Send + Sync + 'static, $($T: 'static,)* R: 'static>(
            f: &$value<$H, $($T,)* R>,
            x: &$H,
        ) -> $prev<$($T,)* R> {
            if STAGED_LIVE.load(Ordering::Acquire) == 0 {
                return $pap(f, x, None);
            }
            if let Some(Entry { step: Some((ty, p)), .. }) = lookup(key_of(f)) {
                if ty == TypeId::of::<$step<$H, $($T,)* R>>() {
                    // SAFETY: STATE-4: the type was checked, and `f` keeps
                    // the environment holding the step alive.
                    let step = unsafe { &*(p as *const $step<$H, $($T,)* R>) }.clone();
                    return step(x);
                }
            }
            $pap(f, x, None)
        }
    };
}

fv_arity!(FnValue0, local0, crossing0, proxy0, IntoCallback0, Invoke0, ());
fv_arity!(FnValue1, local1, crossing1, proxy1, IntoCallback1, Invoke1, (A1 a1));
fv_arity!(FnValue2, local2, crossing2, proxy2, IntoCallback2, Invoke2, (A1 a1, A2 a2));
fv_arity!(FnValue3, local3, crossing3, proxy3, IntoCallback3, Invoke3, (A1 a1, A2 a2, A3 a3));
fv_arity!(FnValue4, local4, crossing4, proxy4, IntoCallback4, Invoke4, (A1 a1, A2 a2, A3 a3, A4 a4));
fv_arity!(FnValue5, local5, crossing5, proxy5, IntoCallback5, Invoke5, (A1 a1, A2 a2, A3 a3, A4 a4, A5 a5));
fv_arity!(FnValue6, local6, crossing6, proxy6, IntoCallback6, Invoke6, (A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6));
fv_arity!(FnValue7, local7, crossing7, proxy7, IntoCallback7, Invoke7, (A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6, A7 a7));
fv_arity!(FnValue8, local8, crossing8, proxy8, IntoCallback8, Invoke8, (A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6, A7 a7, A8 a8));

fv_apply!(FnValue1, FnValue0, Step1, pap1, staged1, apply1_1, fn_ptr1, (A1 a1));
fv_apply!(FnValue2, FnValue1, Step2, pap2, staged2, apply1_2, fn_ptr2, (A1 a1), (A2 a2));
fv_apply!(FnValue3, FnValue2, Step3, pap3, staged3, apply1_3, fn_ptr3, (A1 a1), (A2 a2), (A3 a3));
fv_apply!(FnValue4, FnValue3, Step4, pap4, staged4, apply1_4, fn_ptr4, (A1 a1), (A2 a2), (A3 a3), (A4 a4));
fv_apply!(FnValue5, FnValue4, Step5, pap5, staged5, apply1_5, fn_ptr5, (A1 a1), (A2 a2), (A3 a3), (A4 a4), (A5 a5));
fv_apply!(FnValue6, FnValue5, Step6, pap6, staged6, apply1_6, fn_ptr6, (A1 a1), (A2 a2), (A3 a3), (A4 a4), (A5 a5), (A6 a6));
fv_apply!(FnValue7, FnValue6, Step7, pap7, staged7, apply1_7, fn_ptr7, (A1 a1), (A2 a2), (A3 a3), (A4 a4), (A5 a5), (A6 a6), (A7 a7));
fv_apply!(FnValue8, FnValue7, Step8, pap8, staged8, apply1_8, fn_ptr8, (A1 a1), (A2 a2), (A3 a3), (A4 a4), (A5 a5), (A6 a6), (A7 a7), (A8 a8));

#[cfg(test)]
pub(crate) fn registered_count() -> usize {
    REGISTRY.iter().map(|s| s.read().unwrap_or_else(PoisonError::into_inner).len()).sum()
}
