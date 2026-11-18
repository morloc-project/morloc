from mlc_types._local.types import Box, Pair, Rose


def py_full(x):
    return Box.Full(x)


def py_str_full(s):
    return Box.Full(s)


def py_nested(x):
    return Box.Full(Box.Full(x))


def py_boxes():
    return [Box.Full(1), Box.Empty]


def py_pair(s, i):
    return Pair.Both(s, i)


def py_sum(b):
    match b:
        case Box.Full(x):
            return x
        case _:
            return 0


def py_grow(b):
    match b:
        case Box.Full(x):
            return Box.Full(x + 1)
        case _:
            return b


def py_crate(x):
    return {"item": {"item": x}}


def py_rose(x):
    return Rose.Branch(x, [Rose.Tip(x), Rose.Branch(x, [])])
