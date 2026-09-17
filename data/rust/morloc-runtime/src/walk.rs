//! Explicit-stack traversal of a value under its schema.
//!
//! A walker over a voidstar value visits one node per schema node. Where
//! the schema holds a back-reference the value can be arbitrarily deep,
//! so the traversal keeps its frames on the heap: a container node whose
//! child needs a frame pushes the child, and a resume frame for itself
//! beneath it, and returns; the loop pops and steps until the stack is
//! empty. A subtree with no back-reference under it is stepped by direct
//! call instead, which bounds that recursion by the schema's height and
//! keeps flat values -- a list of strings, a record of numbers -- on a
//! loop with no frames at all.
//!
//! Frames carry the schema node, the value pointer in the walker's own
//! address space, the index of the next child to visit, and one
//! walker-specific field: a destination pointer, a buffer offset, a
//! pretty-print depth. A walker with a post action (a closing bracket, a
//! block to free after its elements) pushes a finish frame beneath the
//! first deferred child so it pops after the whole subtree; when no child
//! is deferred, the post action runs inline instead.

use crate::error::MorlocError;
use crate::schema::Schema;

/// One pending visit.
#[derive(Clone, Copy)]
pub struct Frame<X: Copy> {
    /// The schema node as written in the tree; a step resolves a `Recur`.
    pub schema: *const Schema,
    /// The node's value, in whatever address space the walker reads.
    pub data: *const u8,
    /// The next child to visit; 0 on the first visit.
    pub idx: usize,
    /// Whether this visit runs the node's post action rather than a step.
    pub finish: bool,
    /// Whether the node's finish frame is already on the stack, so a
    /// resumed step must not run the post action inline.
    pub finish_pushed: bool,
    pub x: X,
}

impl<X: Copy> Frame<X> {
    pub fn new(schema: &Schema, data: *const u8, x: X) -> Frame<X> {
        Frame { schema, data, idx: 0, finish: false, finish_pushed: false, x }
    }
}

/// The pending visits, innermost last.
pub struct Stack<X: Copy> {
    v: Vec<Frame<X>>,
}

impl<X: Copy> Default for Stack<X> {
    fn default() -> Self {
        Stack { v: Vec::new() }
    }
}

impl<X: Copy> Stack<X> {
    pub fn new() -> Stack<X> {
        Stack::default()
    }

    /// Push a child to be stepped next.
    pub fn enter(&mut self, schema: &Schema, data: *const u8, x: X) {
        self.v.push(Frame::new(schema, data, x));
    }

    /// Push the continuation of `f` at child `idx`, to run once whatever
    /// is pushed above it has been stepped.
    pub fn resume(&mut self, f: &Frame<X>, idx: usize) {
        self.v.push(Frame { idx, finish: false, finish_pushed: true, ..*f });
    }

    /// Push `f`'s post action, to run once everything above it is done.
    pub fn finish(&mut self, f: &Frame<X>) {
        self.v.push(Frame { finish: true, ..*f });
    }

    pub fn pop(&mut self) -> Option<Frame<X>> {
        self.v.pop()
    }

    /// Drop every pending visit, ending the walk after the current step.
    pub fn clear(&mut self) {
        self.v.clear();
    }
}

/// Whether a child was stepped in place or left on the stack.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Visit {
    Done,
    Deferred,
}

pub trait Walker<X: Copy> {
    /// Visit a node. A container visits its children from `f.idx` on; when
    /// a child is deferred it calls `defer` and returns.
    fn step(&mut self, st: &mut Stack<X>, f: Frame<X>) -> Result<(), MorlocError>;

    /// The node's post action, if it has one.
    fn finish(&mut self, _st: &mut Stack<X>, _f: Frame<X>) -> Result<(), MorlocError> {
        Ok(())
    }

    /// Whether this walker has a post action for the node of `f`.
    fn has_finish(&self, _f: &Frame<X>) -> bool {
        false
    }
}

/// Step frames until the stack is empty.
pub fn run<X: Copy, W: Walker<X>>(w: &mut W, st: &mut Stack<X>) -> Result<(), MorlocError> {
    while let Some(f) = st.pop() {
        if f.finish {
            w.finish(st, f)?;
        } else {
            w.step(st, f)?;
        }
    }
    Ok(())
}

/// Leave child `idx` of `f` on the stack, with `f`'s continuation and
/// post action beneath it, so the child's whole subtree runs before the
/// parent goes on. Returns the parent's frame as it must be seen
/// afterwards, which the caller returns from its step.
pub fn defer<X: Copy, W: Walker<X>>(
    w: &W,
    st: &mut Stack<X>,
    f: &Frame<X>,
    idx: usize,
    child: &Schema,
    data: *const u8,
    x: X,
) {
    if w.has_finish(f) && !f.finish_pushed {
        st.finish(f);
    }
    st.resume(f, idx + 1);
    st.enter(child, data, x);
}

/// Run the post action of `f` inline when a step visited every child in
/// place and no finish frame was pushed for it.
pub fn end_step<X: Copy, W: Walker<X>>(w: &mut W, st: &mut Stack<X>, f: &Frame<X>) -> Result<(), MorlocError> {
    if w.has_finish(f) && !f.finish_pushed {
        w.finish(st, *f)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::recur::Resolver;
    use crate::schema::parse_schema;

    // A walker that records the order in which nodes open and close, so
    // the frame discipline can be checked against a plain recursive walk.
    // A back-reference is reported as a leaf rather than followed, so the
    // walk over the schema alone ends.
    struct Trace<'r> {
        res: Resolver<'r>,
        out: Vec<String>,
    }

    impl<'r> Trace<'r> {
        fn child(&mut self, st: &mut Stack<()>, f: &Frame<()>, idx: usize, s: &'r Schema) -> Result<Visit, MorlocError> {
            if self.res.flat(s) {
                self.step(st, Frame::new(s, std::ptr::null(), ()))?;
                Ok(Visit::Done)
            } else {
                defer(self, st, f, idx, s, std::ptr::null(), ());
                Ok(Visit::Deferred)
            }
        }
    }

    impl<'r> Walker<()> for Trace<'r> {
        fn has_finish(&self, f: &Frame<()>) -> bool {
            let s = unsafe { &*f.schema };
            !s.parameters.is_empty()
        }
        fn step(&mut self, st: &mut Stack<()>, f: Frame<()>) -> Result<(), MorlocError> {
            let s: &'r Schema = unsafe { &*f.schema };
            if f.idx == 0 {
                self.out.push(format!("open {:?}", s.serial_type));
            }
            for i in f.idx..s.parameters.len() {
                if self.child(st, &f, i, &s.parameters[i])? == Visit::Deferred {
                    return Ok(());
                }
            }
            end_step(self, st, &f)
        }
        fn finish(&mut self, _st: &mut Stack<()>, f: Frame<()>) -> Result<(), MorlocError> {
            let s = unsafe { &*f.schema };
            self.out.push(format!("close {:?}", s.serial_type));
            Ok(())
        }
    }

    #[test]
    fn frames_open_and_close_in_recursive_order() {
        // A record with a flat field before and after a deep one: the
        // deep field defers, the flat ones run in place, and every close
        // follows its subtree exactly as a recursive walk would order it.
        let s = parse_schema("&1Rm31ai41b?t2^1Rs1cs").unwrap();
        let mut w = Trace { res: Resolver::new(&s), out: Vec::new() };
        let mut st = Stack::new();
        st.enter(&s, std::ptr::null(), ());
        run(&mut w, &mut st).unwrap();
        let expect = [
            "open Map",
            "open Sint32",
            "open Optional",
            "open Tuple",
            "open Recur",
            "open String",
            "open Uint8",
            "close String",
            "close Tuple",
            "close Optional",
            "open String",
            "open Uint8",
            "close String",
            "close Map",
        ];
        assert_eq!(w.out, expect);
        // The same schema with no back-reference walks with no frame at all:
        // the stack is empty after the root step returns.
        let flat = parse_schema("m21ai41cs").unwrap();
        let mut w = Trace { res: Resolver::new(&flat), out: Vec::new() };
        let mut st = Stack::new();
        w.step(&mut st, Frame::new(&flat, std::ptr::null(), ())).unwrap();
        assert!(st.pop().is_none());
        assert_eq!(w.out, ["open Map", "open Sint32", "open String", "open Uint8", "close String", "close Map"]);
    }
}
