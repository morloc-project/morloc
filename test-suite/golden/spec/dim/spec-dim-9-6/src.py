def mm(a, b):
    (m, k), xs = a
    (_, n), ys = b
    return ((m, n), [sum(xs[i * k + j] * ys[j * n + c] for j in range(k)) for i in range(m) for c in range(n)])

def sq(a):
    return a[0][0]

def first(a):
    return a[1][0]
