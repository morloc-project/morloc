import random

def choose(xs):
    return random.choice(xs)

def roll(n, d):
    return [random.randint(1, d) for _ in range(n)]
