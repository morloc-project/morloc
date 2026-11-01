def count_inner(xs):
    return sum(1 for o in xs if o["inner"] is not None)

count_pinner = count_inner

def count_shape(xs):
    return sum(1 for h in xs if h["shape"] is not None)
