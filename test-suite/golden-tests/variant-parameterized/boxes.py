def py_full(x):
    return ("Full", (x,))


def py_str_full(s):
    return ("Full", (s,))


def py_nested(x):
    return ("Full", (("Full", (x,)),))


def py_boxes():
    return [("Full", (1,)), ("Empty", ())]


def py_pair(s, i):
    return ("Both", (s, i))


def py_sum(b):
    match b:
        case ("Full", (x,)):
            return x
        case _:
            return 0


def py_grow(b):
    match b:
        case ("Full", (x,)):
            return ("Full", (x + 1,))
        case _:
            return b


def py_crate(x):
    return {"item": {"item": x}}
