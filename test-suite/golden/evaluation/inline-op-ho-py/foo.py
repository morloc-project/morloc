def fold(f, b, xs):
    for x in xs:
        b = f(b, x)
    return b
