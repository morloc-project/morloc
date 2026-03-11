import random as _random

def setSeed(seed):
    _random.seed(seed)
    return None

def randomReal():
    return _random.random()
