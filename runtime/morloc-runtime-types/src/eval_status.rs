//! What a failed `morloc eval` or `morloc typecheck` child's wait status
//! means to whoever asked for it.

/// The status a GHC program exits with when it runs out of heap.
pub const EXIT_HEAPOVERFLOW: i32 = 251;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum EvalFailure {
    /// It ran out of its CPU budget.
    Timeout,
    /// It ran out of the heap the server gave it.
    HeapCeiling,
    /// Something other than the expression failed: a signal, an internal
    /// error (PANIC-1).
    Internal,
    /// The wrapper could not start `morloc` (DAEMON-6).
    CouldNotStart,
    /// The expression was rejected or raised an error.
    Rejected,
}

/// The meaning of `status`, a wait status of a child that did not succeed.
pub fn eval_failure(status: libc::c_int) -> EvalFailure {
    if libc::WIFSIGNALED(status) {
        return if libc::WTERMSIG(status) == libc::SIGXCPU { EvalFailure::Timeout } else { EvalFailure::Internal };
    }
    match libc::WIFEXITED(status).then(|| libc::WEXITSTATUS(status)) {
        Some(EXIT_HEAPOVERFLOW) => EvalFailure::HeapCeiling,
        Some(crate::panic::PANIC_EXIT_STATUS) => EvalFailure::Internal,
        Some(126 | 127) => EvalFailure::CouldNotStart,
        _ => EvalFailure::Rejected,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn exited(code: i32) -> libc::c_int {
        (code & 0xff) << 8
    }

    #[test]
    fn a_failed_eval_is_the_expressions_fault_only_when_it_exits_on_its_own() {
        assert_eq!(eval_failure(exited(1)), EvalFailure::Rejected);
        assert_eq!(eval_failure(exited(EXIT_HEAPOVERFLOW)), EvalFailure::HeapCeiling);
        assert_eq!(eval_failure(exited(70)), EvalFailure::Internal);
        assert_eq!(eval_failure(exited(127)), EvalFailure::CouldNotStart);
        assert_eq!(eval_failure(libc::SIGXCPU), EvalFailure::Timeout);
        assert_eq!(eval_failure(libc::SIGSEGV), EvalFailure::Internal);
    }
}
