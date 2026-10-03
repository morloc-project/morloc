import os
import __main__


def drift(procs, reps):
    # The thread model (the macOS default) keeps no count across processes,
    # and forking its live interpreter aborts there.
    if __main__._busy_ref is None:
        return 0
    # The pool's main loop starts workers while the count is high; this
    # probe's own children account for it, so none are wanted.
    before = __main__._busy_ref.value
    children = []
    for _ in range(procs):
        pid = os.fork()
        if pid == 0:
            __main__._wakeup_fd = -1
            for _ in range(reps):
                __main__._tracked_call(lambda: None)
            os._exit(0)
        children.append(pid)
    for pid in children:
        os.waitpid(pid, 0)
    return __main__._busy_ref.value - before
