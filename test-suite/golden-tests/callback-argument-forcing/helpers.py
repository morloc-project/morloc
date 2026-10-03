import os

_LOG = os.path.join(os.path.dirname(os.path.abspath(__file__)), "note.log")


def note(x):
    with open(_LOG, "a") as fh:
        fh.write(str(x) + "\n")
    return x


def py_each(f, xs):
    return [f(x) for x in xs]
