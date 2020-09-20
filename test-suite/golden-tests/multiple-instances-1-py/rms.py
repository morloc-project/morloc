import math

def rms1(xs):
    return math.sqrt(sum([x*x for x in xs]))

def rms2(xs):
    y = 0
    for x in xs:
        y += x*x
    return math.sqrt(y)
