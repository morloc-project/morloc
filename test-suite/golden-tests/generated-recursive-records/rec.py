def chainLength(c):
    n = 0
    while c is not None:
        n += 1
        c = c["next"]
    return n
