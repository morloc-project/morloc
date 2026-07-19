def py_len(s):
    return len(s)

def py_may_fail(s):
    if not s:
        raise ValueError("py_may_fail: empty input")
    return len(s)

def py_add(a, b):
    return a + b

def py_pair(a, b):
    return (a, b)

def py_sum_list(xs):
    return sum(xs)
