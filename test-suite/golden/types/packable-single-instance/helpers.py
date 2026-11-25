def pack_generic(xs):
    return {"v": xs, "how": "generic"}


def unpack_generic(d):
    return d["v"]


def pack_special(xs):
    return {"v": xs, "how": "special"}


def unpack_special(d):
    return d["v"]


def how(d):
    return d["how"]
