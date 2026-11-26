"""Typechecking time against program size. Each program is generated into a
temporary directory and checked with `morloc typecheck`."""

import os
import shutil
import tempfile

from perf import fit_linear, fit_power

HEAD = "module main (g)\n\nimport root-py\n"


def program(body, **extra):
    return {"main.loc": HEAD + "\n" + body + "\n", **extra}


def nest(n, inner, wrap):
    """`wrap` applied n times around `inner`."""
    e = inner
    for _ in range(n):
        e = wrap(e)
    return e


def calls_same(k):
    calls = ", ".join(["f 7"] * k)
    return program(f"f :: U64 -> I64\nf x = tryInto x\n\ng = [{calls}]")


def calls_imported(k):
    calls = ", ".join(["f 7"] * k)
    return program(f"import .lib\n\ng = [{calls}]",
                   **{"lib.loc": "module lib (f)\n\nimport root\n\nf :: U64 -> I64\nf x = tryInto x\n"})


def chain(n):
    defs = ["f0 :: I64 -> I64", "f0 x = x + 1"]
    for i in range(1, n):
        defs += [f"f{i} :: I64 -> I64", f"f{i} x = f{i - 1} (x + {i})"]
    return program("\n".join(defs) + f"\n\ng x = f{n - 1} x")


def ladder(d):
    """Each level names the one below twice: 2^d paths, d definitions."""
    defs = ["f0 :: I64 -> I64", "f0 x = x + 1"]
    for i in range(1, d + 1):
        defs += [f"f{i} :: I64 -> I64", f"f{i} x = f{i - 1} x + f{i - 1} (x + 1)"]
    return program("\n".join(defs) + f"\n\ng x = f{d} x")


ROW = "{a = Real, b = Real, c = Real, d = Real, e = Real, f = Real, g = Real, h = Real}"


def ladder_indexed(d):
    """The ladder over a type indexed by a row count and a column schema."""
    defs = ["type Table (n :: Nat) (r :: Rec)",
            "nrow :: Table n r -> I64",
            "sliceRows :: I64 -> I64 -> Table n r -> Table m r",
            f"p0 :: I64 -> Table m {ROW}"]
    for i in range(1, d + 1):
        defs += [f"p{i} :: I64 -> Table m {ROW}",
                 f"p{i} n = sliceRows 0 (nrow (p{i - 1} n)) (p{i - 1} n)"]
    return program("\n".join(defs) + f"\n\ng :: I64 -> I64\ng n = nrow (p{d} n)")


def box_ladder(d):
    defs = ["type Box (n :: Nat) a",
            "bcat :: Box a Real -> Box b Real -> Box m Real",
            "bsize :: Box a Real -> I64",
            "p0 :: I64 -> Box m Real"]
    for i in range(1, d + 1):
        defs += [f"p{i} :: I64 -> Box m Real", f"p{i} n = bcat (p{i - 1} n) (p{i - 1} n)"]
    return "\n".join(defs)


def ladder_open_sibling(d):
    """The ladder over an index no use fixes, beside a definition whose
    element type nothing fixes."""
    return program(box_ladder(d) + "\n\nlength0 :: [a] -> I64\nnothingHere :: [a]\nnothingHere = []\n"
                   f"\ng :: I64 -> I64\ng n = bsize (p{d} n) + length0 nothingHere")


TALLY = ("class Tally f where\n  tally :: f a -> I64\n\n"
         "instance Tally List where\n  source Py from \"impl.py\" (\"tally\")\n")
TALLY_PY = {"impl.py": "def tally(xs):\n    return len(xs)\n"}


def ladder_stuck_sibling(d):
    """The ladder beside a method use whose type is open until the method is
    expanded."""
    return program(TALLY + "\n" + box_ladder(d) + f"\n\ng :: I64 -> I64\ng n = bsize (p{d} n) + tally []",
                   **TALLY_PY)


def stuck_uses(n):
    """n method uses whose type is open until the method is expanded."""
    uses = " + ".join(["tally []"] * n)
    return program(TALLY + f"\ng :: I64 -> I64\ng n = n + {uses}", **TALLY_PY)


def method_gt(n):
    """n uses of an Ord method in one definition."""
    ops = "\n  && ".join(f"x > {i}" for i in range(n))
    return program(f"g :: I64 -> Bool\ng x = {ops}")


def method_ne(n):
    """n uses of `!=`, whose candidate instances include ones whose bodies
    use `==` again (tuples, lists)."""
    ops = "\n  && ".join(f"x != {i}" for i in range(n))
    return program(f"g :: I64 -> Bool\ng x = {ops}")


ID_I64 = "f :: I64 -> I64\nf x = x\n"


def apply_nested(n):
    return program(ID_I64 + f"\ng :: I64 -> I64\ng x = {nest(n, 'x', lambda e: f'f ({e})')}")


def apply_annotated(n):
    return program(ID_I64 + f"\ng :: I64 -> I64\ng x = {nest(n, 'x', lambda e: f'f ({e} :: I64)')}")


def apply_multiarg(n):
    body = nest(n - 1, "f x 1", lambda e: f"f ({e}) 1")
    return program(f"f :: I64 -> I64 -> I64\nf x y = x\n\ng :: I64 -> I64\ng x = {body}")


def tuple_nested(n):
    """n pairs of a function into a tuple and one out of it."""
    body = nest(n - 1, "f (h x)", lambda e: f"f (h ({e}))")
    return program("f :: (I64, I64) -> I64\nf x = 1\n\nh :: I64 -> (I64, I64)\nh x = (x, x)\n"
                   f"\ng :: I64 -> I64\ng x = {body}")


def compose(n):
    return program("id_ :: I64 -> I64\nid_ x = x\n\ng = " + " . ".join(["id_"] * n))


def compose_poly(n):
    return program("id_ x = x\n\ng = " + " . ".join(["id_"] * n))


def compose_partial(n):
    return program("add :: I64 -> I64 -> I64\nadd x y = x\n\ninc :: I64 -> I64\ninc = add 1\n\ng = "
                   + " . ".join(["inc"] * n))


def where_chain(d):
    """A chain of signed definitions nested in one `where`."""
    defs = ["    q0 :: I64 -> I64", "    q0 x = x"]
    for i in range(1, d + 1):
        defs += [f"    q{i} :: I64 -> I64", f"    q{i} x = q{i - 1} x"]
    return program(f"g :: I64 -> I64\ng n = q{d} n\n  where\n" + "\n".join(defs))


def empty(_):
    return program("g x = x + 1")


# (item, series, generator, sizes). In --check, each series runs once at its
# second-largest size and must finish within CHECK_S.
SERIES = [
    ("TYCK-2", "chain", chain, [100, 200, 400, 800]),
    ("TYCK-3", "calls-same", calls_same, [256, 512, 1024, 2048]),
    ("TYCK-4", "ladder", ladder, [32, 64, 128, 256]),
    ("TYCK-4", "ladder-indexed", ladder_indexed, [32, 64, 128, 256]),
    ("TYCK-4", "ladder-open-sibling", ladder_open_sibling, [32, 64, 128, 256]),
    ("TYCK-4", "ladder-stuck-sibling", ladder_stuck_sibling, [32, 64, 128, 256]),
    ("TYCK-5", "calls-imported", calls_imported, [128, 256, 512, 1024]),
    ("TYCK-6", "method-gt", method_gt, [250, 500, 1000, 2000]),
    ("TYCK-7", "method-ne", method_ne, [250, 500, 1000, 2000]),
    ("TYCK-8", "apply-nested", apply_nested, [200, 400, 800, 1600]),
    ("TYCK-8", "apply-annotated", apply_annotated, [200, 400, 800, 1600]),
    ("TYCK-8", "apply-multiarg", apply_multiarg, [200, 400, 800, 1600]),
    ("TYCK-8", "tuple-nested", tuple_nested, [200, 400, 800, 1600]),
    ("TYCK-9", "compose", compose, [80, 160, 320, 640]),
    ("TYCK-9", "compose-poly", compose_poly, [80, 160, 320, 640]),
    ("TYCK-9", "compose-partial", compose_partial, [80, 160, 320, 640]),
    ("TYCK-10", "where-chain", where_chain, [64, 128, 256, 512]),
    ("TYCK-11", "stuck-uses", stuck_uses, [256, 512, 1024, 2048]),
]

# A program that regresses to exponential time fails its point instead of
# holding the run.
TIMEOUT_S = 120
CHECK_S = 10

CMD = ["morloc", "typecheck", "main.loc"]


def write(tmp, name, x, files):
    d = os.path.join(tmp, f"{name}-{x}")
    os.makedirs(d)
    for fn, text in files.items():
        with open(os.path.join(d, fn), "w") as h:
            h.write(text)
    return d


def run(b):
    tmp = tempfile.mkdtemp(prefix="perf-tc-")
    try:
        d0 = write(tmp, "empty", 0, empty(0))
        t0 = b.sweep("empty", [0], lambda x: CMD, cwd_of=lambda x: d0, timeout=TIMEOUT_S)[0]
        b.record("TYCK-1", "empty", t0 * 1e3, "ms", "one definition, importing root-py")

        for item, name, gen, xs in SERIES:
            if b.quick:
                xs = xs[:2]
            dirs = {x: write(tmp, name, x, gen(x)) for x in xs}
            ys = b.sweep(name, xs, lambda x: CMD, cwd_of=lambda x: dirs[x], timeout=TIMEOUT_S)
            c, slope, r2 = fit_linear(xs, ys)
            b.record(item, f"{name}.c", c * 1e3, "ms", f"r2={r2:.4f}")
            b.record(item, f"{name}.b", slope * 1e3, "ms", f"r2={r2:.4f} per unit")
            # t(x) = t0 + a x^k, with t0 the empty program: k = 1 is linear.
            _, k, r2k = fit_power(xs, [max(y - t0, 1e-4) for y in ys])
            b.record(item, f"{name}.k", k, "", f"r2={r2k:.4f}")
            b.record(item, f"{name}.maxrss", b.points[-1]["maxrss_kib"] / 1024, "MiB",
                     f"at x={xs[-1]}")
    finally:
        shutil.rmtree(tmp)


def check(b):
    """Each series once at its second-largest size. Returns the failures."""
    tmp = tempfile.mkdtemp(prefix="perf-tc-")
    failures = []
    try:
        for _, name, gen, xs in SERIES:
            d = write(tmp, name, xs[-2], gen(xs[-2]))
            failures += b.check_point(name, xs[-2], CMD, d, CHECK_S)
    finally:
        shutil.rmtree(tmp)
    return failures
