#  f :: Int -> Int -> Bool
#  g :: Bool -> Bool -> Bool
#  foo :: Tools -> Int -> Int
def foo(tools, x):
    p1 = tools["f"](x, 2)
    p2 = tools["g"](True, p1 > 2)
    return p2
