def bar(cond):
    if cond:
        return lambda x: 2*x
    else:
        return lambda x: 3*x
