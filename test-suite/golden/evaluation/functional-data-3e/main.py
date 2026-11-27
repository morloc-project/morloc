def bar(cond):
    if cond:
        return [lambda x: 2*x, lambda x: 3*x]
    else:
        return [lambda x: 4*x, lambda x: 5*x]
