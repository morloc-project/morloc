# A payload-bearing `data` value is an instance of its constructor's
# generated class (GEN-12), matched by class pattern.
from mlc_types._local.main import Shape


def area(s):
    match s:
        case Shape.Circle(r):
            return 3.0 * r * r
        case Shape.Rect(w, h):
            return w * h
        case _:
            return 0.0


def grow(s):
    match s:
        case Shape.Circle(r):
            return Shape.Circle(r + 1.0)
        case Shape.Rect(w, h):
            return Shape.Rect(w + 1.0, h + 1.0)
        case _:
            return s
