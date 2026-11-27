def bar(cond1, cond2):
    if cond1:
        return lambda x: 2*x
    else:
        return lambda x: 3*x + cond2

def baz(cond1):
    def f1(num):
        def f2(cond2):
            return cond1 + num + 3*cond2
        return f2
    return f1
