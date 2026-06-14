# Python relative import across the subpackage tree. Verifies the
# exposed copy preserves directory structure end-to-end.
from ..helpers import add_one


def util_add_one(x):
    return add_one(x)


def doubler(x):
    return x * 2
