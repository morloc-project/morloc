import os


def produce_forks(nchildren, big, sink):
    sink([0])
    pids = []
    for c in range(1, nchildren + 1):
        pid = os.fork()
        if pid == 0:
            sink([10 * c + 1, 10 * c + 2])
            sink([10 * c + 3])
            if big > 0:
                sink([7] * big)
            os._exit(0)
        pids.append(pid)
    for pid in pids:
        _, status = os.waitpid(pid, 0)
        if status != 0:
            sink([-1])
    sink([99])


def sorted_ints(xs):
    return "py%s\n" % "".join(" %d" % x for x in sorted(xs))


def count(xs):
    return "py n=%d\n" % len(xs)
