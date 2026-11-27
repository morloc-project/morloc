#  class Reversible ([a],[b]) [(a,b)] where
def forward(x):
    return list(zip(x[0], x[1]))

def backward(xys):
    xs = []
    ys = []
    for (x,y) in xys:
        xs.append(x)
        ys.append(y)
    return (xs, ys)

#  addLen :: Str -> (Str, Int)
def addLen(x):
    return (x, len(x))
