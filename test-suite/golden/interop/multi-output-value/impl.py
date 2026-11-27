def build(log, n):
    with open(log, "a") as f:
        f.write("ran\n")
    print("building", n, flush=True)
    return [i * i for i in range(n)]

def as_lines(xs):
    return "".join("%d\n" % x for x in xs)

def pad(width, xs):
    return "".join(str(x).rjust(width) + "\n" for x in xs)

def sink_lines(xs):
    for x in xs:
        print("sink", x)

def summary(xs):
    return len(xs)
