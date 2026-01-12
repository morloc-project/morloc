#  bar :: Bool -> (Int -> Int, Bool -> Int)
def bar(cond):
    return (lambda x: 2 * x, lambda x: x + 5)
