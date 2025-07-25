import multiprocessing as mp
import os

n_workers = max(1, os.cpu_count() // 2)

def pmap(f, xs):
    with mp.Pool(processes=n_workers) as pool:
        results = pool.map(f, xs)
    return results

def add(x, y):
    return x + y
