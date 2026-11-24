import os


def produce_forks(sink):
    sink([0])
    for c in range(1, 4):
        pid = os.fork()
        if pid == 0:
            code = 0
            try:
                sink([10 * c + 1, 10 * c + 2])
            except BaseException:
                code = 1
            os._exit(code)
        _, status = os.waitpid(pid, 0)
        if os.WEXITSTATUS(status) != 0:
            sink([-1])
    sink([99])


def add_batch(acc, xs):
    return acc + len(xs)


def merge(a, b):
    return a + b


def show_n(n):
    return f"py n={n}\n"


def produce_map_forks(sink):
    sink([{"a": 1}])
    pid = os.fork()
    if pid == 0:
        code = 0
        try:
            sink([{"b": 2}])
        except BaseException:
            code = 3
        os._exit(code)
    os.waitpid(pid, 0)
    sink([{"c": 3}])


def add_maps(acc, xs):
    return acc + len(xs)


def _gather_files():
    d = os.environ.get("MORLOC_TMPDIR")
    if d is None:
        return -1
    n = 0
    for _, _, files in os.walk(d):
        n += sum(1 for f in files if f.startswith("morloc-gather-"))
    return n


def produce_route(sink):
    sink([0, 1])
    sink([])
    sink([2, 3, 4])
    sink([1000 + _gather_files()])


def produce_reals(sink):
    sink([1, 2.5])
    sink([3])


def produce_ints(sink):
    sink([True, 2])
    sink([3])


def produce_pairs(sink):
    sink([[1, "a"], (2, "b")])


def produce_hits(sink):
    sink([{"score": 1, "name": "a"}, {"name": "b", "score": 2}])


def produce_order(sink):
    sink([0])
    for c in range(1, 4):
        pid = os.fork()
        if pid == 0:
            code = 0
            try:
                sink([10 * c + 1])
                sink([10 * c + 2])
            except BaseException:
                code = 3
            os._exit(code)
        _, status = os.waitpid(pid, 0)
        if os.WEXITSTATUS(status) != 0:
            sink([-1])
        sink([50 + c])
    sink([99])


def show_forms(xs):
    return "py " + repr(xs) + " " + " ".join(type(x).__name__ for x in xs) + "\n"


STRS = ["a", "é", "日本", "\U0001f600"]


def produce_strs(sink):
    sink(STRS)
    pid = os.fork()
    if pid == 0:
        code = 0
        try:
            sink(STRS)
        except BaseException:
            code = 1
        os._exit(code)
    os.waitpid(pid, 0)


def show_ascii(xs):
    return "py " + ascii(xs) + " " + " ".join(type(x).__name__ for x in xs) + "\n"
