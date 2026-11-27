import pathlib

def shout(s):
    return s.upper()

def bump(n):
    return n + 1

def rev(xs):
    return list(reversed(xs))

def swap(p):
    return (p[1], p[0])

def to_path(s):
    return pathlib.Path(s)

def from_path(p):
    return str(p)

def suffix(p):
    return p.suffix


def decode_utf8(b):
    # only valid if the value really is `bytes`, as the module declares
    return b.decode("utf-8")
