import struct

sys = 5


def m1(x):
    return x


def py_count(n):
    return m1(n + sys + len(struct.pack("<q", 0)))
