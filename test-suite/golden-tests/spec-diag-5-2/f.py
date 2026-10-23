def pyThrowAtZero(n):
    if n == 0:
        raise ZeroDivisionError("pyThrowAtZero: n must be non-zero")
    return 10.0 / n


def pyDivide(a, b):
    if b == 0:
        raise ZeroDivisionError("pyDivide: b must be non-zero")
    return a / b
