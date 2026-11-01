import sys
def py_f(x): return x + 1
def py_map(f, xs): return [f(x) for x in xs]
def py_sum(xs): return sum(xs)
def py_tick(n):
    print("tick", file=sys.stderr)
    return n
def py_make_adder(k):
    print("made", file=sys.stderr)
    return lambda x: x + k
def py_feed(k): return k(lambda x: 3 * x)
