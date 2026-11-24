def mk_hists(k, seed):
    return [list(range(i + seed, i + seed + 256)) for i in range(k)]


def summ(xs):
    return f"{len(xs)} {sum(h[7] for h in xs)}"


def count_hists(xs):
    return len(xs)
