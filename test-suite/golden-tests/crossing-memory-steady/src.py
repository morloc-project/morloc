def readings(i):
    # Long enough that a batch travels inside its packet rather than by
    # reference, which is the shape that shows up in ordinary work.
    return list(range(i, i + 3000))
