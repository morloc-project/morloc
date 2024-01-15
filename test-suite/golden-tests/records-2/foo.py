#  packMap   Py :: pack   => [(a,b)] -> Map a b
def packMap(xs):
    return dict(xs)

#  unpackMap Py :: unpack => Map a b -> [(a,b)]
def unpackMap(x):
    return list(x.items())

#  push :: (n -> n')
#       -> (n' -> e -> n -> (e', n'))
#       -> (n' -> e -> l -> (e', l'))
#       -> Map n (e, l)
#       -> Map n' (e', l')
def push(f, g, h, x):
    if not x:
        return x

    xs = list(x.items())
    a = f(xs[0][1])
    ys = dict()
    for n, (e, l) in x.items():
        (e2, n2) = g(a, e, n)
        (e3, l2) = h(a, e2, l)
        ys[n2] = (e3, l2)

    return ys

#  makeMap :: Str -> Int -> Map Str (Int, Str)
def makeMap(k, e):
    return {k : (e, "Alice")}
