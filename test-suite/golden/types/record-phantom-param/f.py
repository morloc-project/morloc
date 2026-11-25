def total(xs):
    return sum(x["src"] + x["dst"] for x in xs)
