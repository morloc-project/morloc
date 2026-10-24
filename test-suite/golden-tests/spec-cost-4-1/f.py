def left_fold(f, b, xs):
    for x in xs:
        b = f(b, x)
    return b

def plus(a, b):
    return a + b

def show(x):
    return str(x)
