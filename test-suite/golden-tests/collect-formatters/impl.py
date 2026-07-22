def make_ints(i):
    return [i * 10 + 3, i * 10 + 1, i * 10 + 2]

def sort_ints(xs):
    return sorted(xs)

def tag_ints(xs):
    return xs + [len(xs)]

def tag_ints_off(off, xs):
    return [off] + xs

def render_ints(xs):
    return " ".join(str(x) for x in xs) + " \n"

def render_ints_off(off, xs):
    return "[" + str(off) + "]" + "".join(" " + str(x) for x in xs) + "\n"

def make_strs(i):
    return ["b" + str(i), "a" + str(i)]

def sort_strs(xs):
    return sorted(xs)

def join_strs(xs):
    return ";".join(xs) + ";\n"
