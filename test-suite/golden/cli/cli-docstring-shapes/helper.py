def py_id(x):
    return x


def py_size(xs):
    return len(xs)


def py_sum_int(xs):
    return sum(xs)


def py_sum_float(xs):
    return float(sum(xs))


def py_fst_int(t):
    return t[0]


def py_rec_xs_len(opts):
    return len(opts["xs"])


def py_rec_ys_sum(opts):
    return sum(opts["ys"])


def py_rec_dna_len(opts):
    return len(opts["dna"])


def py_rec_pathr_len(opts):
    return len(opts["pathr"])
