#  bar :: Bool -> (Int -> Int, Bool -> Int)
def bar(cond):
    return (lambda x: 2 * x, lambda x: x + 5)

def fst(xs):
    return xs[0]

def snd(xs):
    return xs[1]
