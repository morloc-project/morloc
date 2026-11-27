import os


def produce(sink):
    sink([1])


def produce_route(sink):
    root = os.environ.get("MORLOC_TMPDIR")
    n = 0
    if root is not None:
        for _, _, files in os.walk(root):
            n += sum(1 for f in files if f.startswith("morloc-gather-"))
    sink([1000 + n])


def add_sum(acc, xs):
    return acc + sum(xs)


def merge(a, b):
    return a + b


def show_n(n):
    return f"py n={n}\n"


def dec(n):
    return n - 1


def is_zero(n):
    return n == 0
