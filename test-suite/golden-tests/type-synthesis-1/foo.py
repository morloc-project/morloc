#  foo :: Int -> Int
def foo(x):
    return x + 1

#  foos :: Int -> [Int]
def foos(x):
    return [x, x]

#  sfoos :: [Int] -> (Int, [Int])
def sfoos(xs):
    return (1, xs)

#  toPair a :: a -> (a, a)
def toPair(x):
    return (x,x)

#  swapPair a b :: (a, b) -> (b, a)
def swapPair(xs):
    return (xs[1], xs[0])
