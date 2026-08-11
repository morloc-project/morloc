def sum_ints(xs):
    return sum(xs)

def count_words(xs):
    return len(xs)

def config_summary(cfg):
    return f"{cfg['tmpDir']}:{cfg['numThreads']}:{cfg['removeCaches']}"

def map_size(m):
    return len(m)

def read_lines(path):
    with open(path) as fh:
        return [line.rstrip("\n") for line in fh]

def print_each(xs):
    for x in xs:
        print(x)

def join_semi(xs):
    return ";".join(xs)
