import multiprocessing

def pmap(f, xs):
    with multiprocessing.Pool() as pool:
        results = pool.map(f, xs)
    return results

def smap(f, xs):
    return [f(x) for x in xs]
