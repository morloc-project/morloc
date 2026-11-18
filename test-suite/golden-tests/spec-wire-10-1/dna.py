from mlc_types._local.main import DNA


def complement_all(xs):
    return [DNA(3 - b.value) for b in xs]
