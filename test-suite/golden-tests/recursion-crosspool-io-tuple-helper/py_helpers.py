def py_step(xs):
    # drop the head, return it as the scalar contribution
    if len(xs) == 0:
        return (xs, 0)
    return (xs[1:], xs[0])


def py_log(xs):
    import sys
    print("py step over", xs, file=sys.stderr)
