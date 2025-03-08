import random
import math
import multiprocessing


def simulate(n: int) -> list[float]:
    return [random.normalvariate(mu=0, sigma=1) for _ in range(n)]

def mean(xs: list[float]) -> float:
    return sum(xs) / len(xs)

def sd(xs: list[float]) -> float:
    mu = mean(xs)
    return math.sqrt(sum([(x - mu) * (x - mu) for x in xs]) / (len(xs) - 1))

def nTrials(n: int, a, f):
    with multiprocessing.Pool() as pool:
        results = pool.map(f, (a for _ in range(n)))
    return results
