def produce_none(sink):
    pass


def produce_fail(sink):
    sink([1, 2])
    sink([3])
    raise RuntimeError("producer failed after 2 batches")


def produce_ten(sink):
    sink([0, 1, 2])
    sink([])
    sink([3, 4, 5, 6, 7, 8, 9])


def show_ints(xs):
    return "py %d:%s\n" % (len(xs), "".join(" %d" % x for x in xs))


def mark_ran(xs):
    with open("handler-ran", "w") as f:
        f.write("%d\n" % len(xs))
    return "handler ran\n"


_stash = []


def produce_stash(sink):
    sink([1, 2])
    _stash.append(sink)


def fire_late():
    for sink in _stash:
        sink([3])
