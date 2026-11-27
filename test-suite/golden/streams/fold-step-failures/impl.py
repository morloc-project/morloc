class Uncopyable(int):
    def __deepcopy__(self, memo):
        raise RuntimeError("init cannot be copied")


def uncopyable():
    return Uncopyable(0)


def produce_caught(sink):
    try:
        sink([1])
    except RuntimeError:
        pass


def add_sum(acc, xs):
    return acc + sum(xs)


def merge(a, b):
    return a + b


def show_n(n):
    return f"py n={n}\n"
