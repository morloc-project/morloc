import os


def node():
    return os.environ.get("MOCK_NODE", "parent")


def here(x):
    return node() + ":" + str(x)


def join2(a, b):
    return "foo@" + node() + "(" + a + "," + b + ")"


def tag(s):
    return "tag@" + node() + "[" + s + "]"


def boom(x):
    raise ValueError("boom " + str(x))


def here_io(x):
    return here(x)


def both(f):
    return f() + "|" + f()
