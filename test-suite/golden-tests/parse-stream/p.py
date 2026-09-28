import os


def produceP(path, sink):
    batch = []
    with open(path) as fh:
        for line in fh:
            line = line.strip()
            if line == "bad":
                raise ValueError("bad line in " + path)
            if line == "die":
                os._exit(3)
            batch.append(int(line))
            if len(batch) == 2:
                sink(batch)
                batch = []
    if batch:
        sink(batch)
    with open(path + ".done", "w") as fh:
        fh.write("done\n")


def doneP(path):
    return os.path.exists(path + ".done")


def sumP(xs):
    return sum(xs)
