def readlines(path):
    with open(path, "r") as f:
        return [line.rstrip("\n") for line in f]


def print_each(xs):
    for line in xs:
        print(line)


def print_csv(xs):
    print(",".join('"' + x + '"' for x in xs))


def join_semi(xs):
    return ";".join(xs)


def describe_int(n):
    return "int(" + str(n) + ")"
