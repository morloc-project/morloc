import enum
from mlc_types._local.main import Base, Shape, Rose, Opt


def describe(b, s):
    return " ".join([
        type(b).__name__, b.name, str(b.value), str(isinstance(b, enum.Enum)),
        type(s).__qualname__, str(isinstance(s, Shape)), str(s is Shape.Empty),
    ])


def describe_shapes(xs):
    out = []
    for s in xs:
        match s:
            case Shape.Circle(r):
                out.append("circle " + str(r))
            case Shape.Rect(w, h):
                out.append("rect " + str(w * h))
            case Shape.Empty:
                out.append("empty " + str(s is Shape.Empty))
    return out


def rose_depth(r):
    match r:
        case Rose.Rose(_, kids):
            return 1 + max([rose_depth(k) for k in kids], default=0)


def opt_text(x):
    match x:
        case Opt.Some(n):
            return "some " + str(n)
        case Opt.None_:
            return "none"


def bad_shape(k):
    return ("Circle", (float(k),))
