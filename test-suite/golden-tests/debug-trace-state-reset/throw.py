def pyThrowAtZero(n):
    if n == 0:
        raise ZeroDivisionError("n was zero")
    return 1.0 / n
