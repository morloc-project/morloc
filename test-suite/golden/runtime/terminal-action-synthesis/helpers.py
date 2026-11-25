def shout(s):
    return s.upper()

def tagWith(n, s):
    return str(n) + ":" + s

def readEf(i):
    return "ef"

def twice(n):
    return [n, n]

def render2(n, xs):
    return str(n) + ":" + str(sum(xs)) + "\n"

def renderAll(xs):
    return "all:" + str(sum(xs))

def renderBatch(xs):
    return "batch:" + str(sum(xs))

def pairUp(n):
    return [n, n]

def renderRows(xss):
    return "rows:" + str(sum(sum(xs) for xs in xss))

def nestTwice(n):
    return [[n, n]]
