import sys
def py_f(x): return x + 1
def py_apply(f, x): return f(x)
def py_feed(g): return g(lambda x: 3 * x)
def py_make_adder(k):
    print("made in py", file=sys.stderr)
    return lambda x: x + k
def py_map(f, xs): return [f(x) for x in xs]
def py_use(f): return f(3)(4)
def py_use_box(b): return b["f"](1)
