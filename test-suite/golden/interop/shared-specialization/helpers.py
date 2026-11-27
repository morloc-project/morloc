import os


def tick(x):
    with open(os.environ["TICK_LOG"], "a") as fh:
        fh.write("tick\n")
    return x


def ticks(_):
    with open(os.environ["TICK_LOG"]) as fh:
        return len(fh.readlines())


def py_id(x):
    return x


def py_f(x):
    return x + 1


def py_map(f, xs):
    return [f(x) for x in xs]


def py_sum(xs):
    return sum(xs)


def py_add(a, b):
    return a + b
