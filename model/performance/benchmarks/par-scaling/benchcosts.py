"""Cost functions for the parallel benchmarks (see parallel/bench/main.loc)."""


def spin(n):
    x = 1
    for _ in range(n):
        x = (x * 1103515245 + 12345) & 0x7FFFFFFF
    return x % 1000


def spin_keep(n):
    return spin(n) % 2 == 0


def spin_list(n):
    v = spin(n)
    return [v] * (v % 4)


def make_str(n):
    return "x" * n


def str_cost(s):
    return len(s) % 1000
