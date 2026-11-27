"""Generate random move/copy programs for goldens own7-fuzz-cpp and
own7-fuzz-rust: chains of functions that keep, forward, alias or recurse
on a parameter, called by a random caller shape. Run from the golden
directory:

  python3 ../own7-matrix-cpp/fuzz.py LANG SEED COUNT

Each program prints [value, payloads copied]; the Makefile and check.py
are the matrix's (gen.py), comparing a run with the checked-in no-move
oracle (oracle.txt, rebuilt by `make oracle`).
"""

import random
import sys

sys.path.insert(0, __file__.rsplit("/", 1)[0])
import gen  # noqa: E402  (only its header and Makefile writers)

LANG, SEED, COUNT = sys.argv[1], int(sys.argv[2]), int(sys.argv[3])
rng = random.Random(SEED)

KINDS = {k: gen.KINDS[k] for k in ("P", "R", "T", "L", "N")}


def body(shape, f, prev, dflt, rd):
    if shape == "keep":
        return "? j > 0 = x : " + dflt
    if shape == "rec":
        return "? j > 1 = %s (j - 2) x : ? j > 0 = x : %s" % (f, dflt)
    if shape == "pass":
        return "%s j x" % prev
    if shape == "passKeep":
        return "? j > 1 = %s (j - 1) x : x" % prev
    if shape == "alias":
        return "(let y = x in ? j > 0 = y : %s)" % dflt
    if shape == "readKeep":
        return "(let a = %s in ? a + j > 0 = x : %s)" % (rd.format(x="x"), dflt)
    if shape == "drop":
        return "(let a = %s in ? j > 9 = x : %s)" % (rd.format(x="x"), dflt)
    raise ValueError(shape)


def caller(shape, chain, mk, rk):
    # every function of the chain, applied innermost first
    def call(a):
        for f in chain:
            a = "%s j (%s)" % (f, a)
        return a
    return {
        "tmp": "%s (%s)" % (rk, call(mk)),
        "last": "(let x = %s in %s (%s))" % (mk, rk, call("x")),
        "live": "(let x = %s in %s (%s) + %s x)" % (mk, rk, call("x"), rk),
        "liveSeq": "(let x = %s in let a = %s x in a + %s (%s))" % (mk, rk, rk, call("x")),
        "twice": "(let x = %s in %s (%s) + %s (%s))" % (mk, rk, call("x"), rk, call("x")),
        "nested": "%s (%s)" % (rk, call(call(mk))),
        "pap": "sum (map %s (map (%s j) [%s]))" % (rk, chain[-1], call(mk) if len(chain) > 1 else mk),
    }[shape]


def program(i):
    k = rng.choice(sorted(KINDS))
    t, mk, rd, dflt, _ = KINDS[k]
    mk = gen.fmt(mk, n="n")
    dflt = gen.fmt(dflt)
    rk = "(\\x -> " + gen.fmt(rd, x="x") + ")"
    lines = ["-- p%03d: kind %s" % (i, k)]
    prev = None
    names = []
    for d in range(rng.randint(1, 3)):
        f = "p%03dg%d" % (i, d)
        shapes = ["keep", "rec", "alias", "readKeep", "drop"] + (["pass", "passKeep"] * 2 if prev else [])
        shape = rng.choice(shapes)
        lines.append("--   %s: %s" % (f, shape))
        lines.append("%s :: Int -> %s -> %s" % (f, t, t))
        lines.append("%s j x = %s" % (f, body(shape, f, prev, dflt, rd)))
        prev = f
        names.append(f)
    cshape = rng.choice(["tmp", "last", "live", "liveSeq", "twice", "nested", "pap"])
    j = rng.randint(0, 3)
    lines.insert(1, "--   caller %s, j=%d" % (cshape, j))
    name = "p%03d" % i
    lines.append("%s :: Int -> Int -> (Int, Int)" % name)
    lines.append("%s n j = census (%s)" % (name, caller(cshape, names, mk, rk)))
    lines.append("")
    return name, j, lines


def main():
    gen.LANG = LANG
    names, js, out = [], {}, []
    for i in range(COUNT):
        name, j, lines = program(i)
        names.append(name)
        js[name] = j
        out += lines
    text = gen.header(names).replace("the OWN-7 move/copy matrix", "random OWN-7 move/copy programs (seed %d)" % SEED)
    open("main.loc", "w").write(text + "\n".join(out))
    gen.write_makefile(".", [(n, n, js[n]) for n in names])
    print(len(names), "programs")


main()
