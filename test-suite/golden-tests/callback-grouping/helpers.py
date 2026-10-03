def make_and_apply(g, x):
    return g(x)(x)


def make_and_apply3(g, x):
    return g(x)(x)(x)


def pass_curried(k):
    return k(lambda a: lambda b: a + b)
