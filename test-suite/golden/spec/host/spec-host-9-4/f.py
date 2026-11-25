import sys
def py_f(x): return x + 1
def py_map(f, xs): return [f(x) for x in xs]
def py_sum(xs): return sum(xs)
def py_make_adder(k):
    print("made in py", file=sys.stderr)
    return lambda x: x + k
def py_relay(f):
    return f
