import sys


class FakeArray:
    @property
    def __array_interface__(self):
        return {"shape": (3,), "typestr": "|u1", "data": (0, True), "version": 3}


class GrowsOnCompare:
    """A dict key equal to "items" whose every comparison grows `items`, so
    the value is larger each time a walk looks the field up."""

    def __init__(self, items):
        self.items = items

    def __hash__(self):
        return hash("items")

    def __eq__(self, other):
        self.items.extend([7] * 4096)
        return other == "items"


def apply_once(f, x):
    return f(x)


def numpy_scalar(n):
    import numpy as np
    return np.uint8(n)


def fake_array(n):
    return FakeArray()


def masked_array(n):
    import numpy as np
    return np.ma.array(np.arange(n, dtype=np.uint8), mask=[i % 2 for i in range(n)])


def numpy_array(n):
    import numpy as np
    return np.arange(n, dtype=np.uint8)


def numpy_subclass(n):
    import numpy as np

    class Tagged(np.ndarray):
        pass

    return np.arange(n, dtype=np.uint8).view(Tagged)


def strs(n):
    return ["s" + str(i) for i in range(n)]


def records(n):
    return [{"name": "r" + str(i), "items": [i]} for i in range(n)]


def missing_key(n):
    return [{"name": "r"}]


def growing(n):
    items = list(range(n))
    return [{GrowsOnCompare(items): items, "name": "g"}]


class ClearsOnCompare:
    """A dict key equal to "items" whose comparison empties `outer`."""

    def __init__(self, outer):
        self.outer = outer

    def __hash__(self):
        return hash("items")

    def __eq__(self, other):
        self.outer.clear()
        return other == "items"


def shrinking(n):
    outer = []
    outer.extend({ClearsOnCompare(outer): [i], "name": "s"} for i in range(n))
    return outer


def numpy_loaded(n):
    return 1 if "numpy" in sys.modules else 0
