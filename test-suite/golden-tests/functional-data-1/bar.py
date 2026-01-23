#  f :: Int -> Int -> Bool
#  g :: Bool -> Bool -> Bool
#  bar :: Tools -> Int -> Int
def bar(tools, x):
    p1 = tools["f"](x, 2)
    p2 = tools["g"](True, p1 > 2)
    return p2
