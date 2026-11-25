def h_id(x): return x
def h_slice(a,b,c,xs): return (xs[0], xs[1][a:b:c])
def h_map_general(f, xs): return (xs[0], [f(x) + 100 for x in xs[1]])
def h_map_int(f, xs): return (xs[0], [f(x) + 200 for x in xs[1]])
