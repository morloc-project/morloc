# Producers of and arithmetic on non-finite IEEE-754 values, used to verify
# that morloc's wire format faithfully transfers Inf/-Inf/NaN to and from
# Python pools, and that Python's IEEE-754 arithmetic produces the
# spec-mandated results.

def fact(n):
    # Reproduces the report's `f 200` factorial-overflow example.
    # Multiplying as float so the overflow happens in IEEE-754, not in
    # Python's arbitrary-precision int.
    r = 1.0
    for i in range(1, n + 1):
        r *= i
    return r

def pyInf(_x):
    return float('inf')

def pyNegInf(_x):
    return float('-inf')

def pyNaN(_x):
    return float('nan')

def add(a, b):
    return a + b

def sub(a, b):
    return a - b

def mul(a, b):
    return a * b

def neg(a):
    return -a
