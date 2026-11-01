def freeze(xs):
    return tuple(xs)


def thaw(t):
    return list(t)


def kind(t):
    return type(t).__name__ + " " + str(len(t))
