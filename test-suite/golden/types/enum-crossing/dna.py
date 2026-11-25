# The generated DNA is an enum.Enum whose member values are the ordinals, so
# a member is built back from an ordinal by calling the class.
from mlc_types._local.main import DNA


def complement(x):
    return DNA(3 - x.value)


def complement_all(xs):
    return [DNA(3 - b.value) for b in xs]


def opt_id(x):
    return x
