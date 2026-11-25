def count(n):
    k = 0
    while n is not None:
        k += 1
        n = n.get("next")
    return k
