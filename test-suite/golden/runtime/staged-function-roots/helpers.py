import os


def _log():
    return os.environ["TICK_LOG"]


def tick(x):
    with open(_log(), "a") as fh:
        fh.write("tick\n")
    return x


def _count():
    if not os.path.exists(_log()):
        return 0
    with open(_log()) as fh:
        return len(fh.readlines())


def ticks(_):
    return _count()


def ticks_l(_):
    return _count()


def host_map(f, xs):
    return [f(x) for x in xs]


def host_map2(f, xs, ys):
    return [f(x, y) for x, y in zip(xs, ys)]
