import numpy


def _form(x):
    if isinstance(x, numpy.ndarray):
        flag = "ro" if not x.flags.writeable else "rw"
        if x.dtype == object:
            inner = ",".join(_form(e) for e in x)
        else:
            inner = ",".join(repr(e.item()) for e in x)
        return "ndarray<%s,%s>[%s]" % (x.dtype, flag, inner)
    if isinstance(x, (list, tuple, set, frozenset)):
        items = sorted(x, key=repr) if isinstance(x, (set, frozenset)) else x
        return "%s[%s]" % (type(x).__name__, ",".join(_form(e) for e in items))
    if isinstance(x, dict):
        return "dict{%s}" % ",".join("%s:%s" % (_form(k), _form(v)) for k, v in x.items())
    if hasattr(x, "__dict__"):
        return "%s%s" % (type(x).__name__, _form(vars(x)))
    return "%s(%r)" % (type(x).__name__, x)


def same(name, a, b):
    fa, fb = _form(a), _form(b)
    if fa == fb:
        return name + " ok"
    return "%s DIFF literal=%s received=%s" % (name, fa, fb)


def echo(x):
    return x
