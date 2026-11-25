def doubleAll(xs):
    return [2 * x if x is not None else 0 for x in xs]


def countNonNull(xs):
    return sum(1 for x in xs if x is not None)


def identityList(xs):
    return list(xs)
