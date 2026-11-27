def pack_rec(r):
    return {"v": [r["left"]], "how": "rec"}


def unpack_rec(d):
    return {"left": d["v"][0], "right": d["how"]}


def how(d):
    return d["how"]
