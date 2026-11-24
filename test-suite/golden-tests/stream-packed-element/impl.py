# The native form is a dict; the wire form is an int. A handler given the
# wire form fails on the subscript.


def pack_tally(x):
    return {"v": x}


def unpack_tally(t):
    return t["v"]


def produce(sink):
    sink([pack_tally(1), pack_tally(2)])
    sink([])
    sink([pack_tally(3)])


def show_all(xs):
    return "py " + " ".join(str(t["v"]) for t in xs) + "\n"


def show_batch(xs):
    return "py batch %d\n" % len([t["v"] for t in xs])
