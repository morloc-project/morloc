# The generated DNA is an enum.IntEnum, so ordinal arithmetic is available
# directly and the boundary coerces the returned int back into a member.
# Int-backed is what keeps a [DNA] a compact buffer rather than a list of
# objects.


def complement(x):
    return 3 - int(x)


def complement_all(xs):
    return [3 - int(b) for b in xs]


def opt_id(x):
    return x
