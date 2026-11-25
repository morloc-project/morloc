import os
import threading

ZERO = {}


def zero_counts():
    return ZERO


def produce_none(sink):
    pass


def produce_fork_none(sink):
    pid = os.fork()
    if pid == 0:
        os._exit(0)
    os.waitpid(pid, 0)


def produce_threads(sink):
    def run(t):
        for b in range(5):
            sink([t * 100 + k for k in range(b + 1)])

    ts = [threading.Thread(target=run, args=(t,)) for t in range(4)]
    for t in ts:
        t.start()
    for t in ts:
        t.join()


def count_into(acc, xs):
    for x in xs:
        acc[x % 10] = acc.get(x % 10, 0) + 1
    return acc


def merge_counts(a, b):
    out = dict(a)
    for k, v in b.items():
        out[k] = out.get(k, 0) + v
    return out


def show_counts(d):
    return "py " + ",".join(f"{k}:{d[k]}" for k in sorted(d)) + "\n"


def add_sum(acc, xs):
    return acc + sum(xs)


def merge(a, b):
    return a + b


def show_n(n):
    return f"py n={n}\n"
