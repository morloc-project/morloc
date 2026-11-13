def gen_reals(n):
    return [(i + 1) * 0.5 for i in range(n)]


def gen_strs(n):
    return ["ACGT"[i % 4] * (8 + i % 17) for i in range(n)]


def gen_tups(n):
    return [(i, i * 0.25, i * 1.5) for i in range(n)]


def cnt(xs):
    return len(xs)
