import random as _random

def setSeed(seed):
    _random.seed(seed)
    return None

def permute(xs):
    ys = list(xs)
    _random.shuffle(ys)
    return ys
