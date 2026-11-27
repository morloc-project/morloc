_tally = {"v": 0}


def gen_small():
    return 1


def add_one(x):
    return x + 1


def risky(x):
    return x * 2


def read_value():
    return 10


def for_each(xs, f):
    for x in xs:
        f(x)


def tap(x):
    _tally["v"] += x


def total_so_far():
    return _tally["v"]


def reset_tally():
    _tally["v"] = 0
