# A Python form of a `data` type is not used: the value is an instance of
# the type's generated class whatever the mapping says (GEN-12).
from mlc_types._local.types import Box


def grow(b):
    match b:
        case Box.Full(x):
            return Box.Full(x + 100)
        case _:
            return b


def bump(w):
    return {"item": w["item"] + 100}
