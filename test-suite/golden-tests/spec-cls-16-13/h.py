def h_id(x): return x
def h_slice(a,b,c,xs): return xs[a:b:c]
def h_map_general(f, xs): return [f(x) + 100 for x in xs]
def h_map_three(f, xs): return [f(x) + 300 for x in xs]
