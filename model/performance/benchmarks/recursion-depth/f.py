def depth(n):
    return 0 if n == 0 else 1 + depth(n - 1)
