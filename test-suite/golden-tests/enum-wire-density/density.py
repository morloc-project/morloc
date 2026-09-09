# A [DNA] arrives as a compact buffer of one-byte ordinals -- a numpy uint8
# array where numpy is available, otherwise bytes -- not a list of IntEnum
# members. Constructing one Python object per base would defeat the purpose
# of the tier, so nothing here indexes the buffer element by element.


def byte_size(xs):
    view = memoryview(xs)
    return view.nbytes


def count_purines(xs):
    # A and G are ordinals 0 and 2.
    return sum(1 for b in memoryview(xs).tolist() if b in (0, 2))


def head_base(xs):
    return memoryview(xs).tolist()[0]


def identity(xs):
    return xs
