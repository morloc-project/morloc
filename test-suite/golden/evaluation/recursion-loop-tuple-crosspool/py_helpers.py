def py_step(xs):
    # advance the list (drop the head), return the dropped head as the scalar
    # contribution -> state is a (list, scalar) tuple
    if len(xs) == 0:
        return (xs, 0)
    return (xs[1:], xs[0])


def py_step3(xs):
    # 3-tuple variant: (rest, head, head) so the caller can sum two scalars
    if len(xs) == 0:
        return (xs, 0, 0)
    return (xs[1:], xs[0], xs[0])


def py_log(xs):
    # a Python-side IO side effect; writes to stderr so stdout stays clean
    import sys
    print("py step over", xs, file=sys.stderr)
