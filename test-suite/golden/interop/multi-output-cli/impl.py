def build(n):
    return list(range(n))

def as_csv(xs):
    return ",".join(str(x) for x in xs) + "\n"

def as_table(xs):
    return "".join("row %d\n" % x for x in xs)

def boom(xs):
    print("partial", flush=True)
    raise ValueError("boom")
