def py_id(x):
    return x


def py_size(xs):
    return len(xs)


def py_sum_int(xs):
    return sum(xs)


def py_sum_real(xs):
    return float(sum(xs))


def py_total_bytes(xss):
    return sum(len(b) for b in xss)
