"""Typechecking time against program size. Each program is generated into a
temporary directory and checked with `morloc typecheck`."""

import os
import shutil
import tempfile

from perf import fit_linear, fit_power

HEAD = "module main (g)\n\nimport root-py\n"


def calls_same(k):
    calls = ", ".join(["f 7"] * k)
    return {"main.loc": HEAD + f"\nf :: U64 -> Int\nf x = tryInto x\n\ng = [{calls}]\n"}


def calls_imported(k):
    calls = ", ".join(["f 7"] * k)
    return {"lib.loc": "module lib (f)\n\nimport root\n\nf :: U64 -> Int\nf x = tryInto x\n",
            "main.loc": HEAD + f"import .lib\n\ng = [{calls}]\n"}


def chain(n):
    defs = ["f0 :: Int -> Int", "f0 x = x + 1"]
    for i in range(1, n):
        defs += [f"f{i} :: Int -> Int", f"f{i} x = f{i - 1} (x + {i})"]
    return {"main.loc": HEAD + "\n" + "\n".join(defs) + f"\n\ng x = f{n - 1} x\n"}


def ladder(d):
    """Each level names the one below twice: 2^d paths, d definitions."""
    defs = ["f0 :: Int -> Int", "f0 x = x + 1"]
    for i in range(1, d + 1):
        defs += [f"f{i} :: Int -> Int", f"f{i} x = f{i - 1} x + f{i - 1} (x + 1)"]
    return {"main.loc": HEAD + "\n" + "\n".join(defs) + f"\n\ng x = f{d} x\n"}


def empty(_):
    return {"main.loc": HEAD + "\ng x = x + 1\n"}


def write(tmp, name, x, files):
    d = os.path.join(tmp, f"{name}-{x}")
    os.makedirs(d)
    for fn, text in files.items():
        with open(os.path.join(d, fn), "w") as h:
            h.write(text)
    return d


def run(b):
    tmp = tempfile.mkdtemp(prefix="perf-tc-")
    cmd = ["morloc", "typecheck", "main.loc"]
    try:
        d0 = write(tmp, "empty", 0, empty(0))
        t0 = b.sweep("empty", [0], lambda x: cmd, cwd_of=lambda x: d0)[0]
        b.record("TYCK-1", "empty", t0 * 1e3, "ms", "one definition, importing root-py")

        series = [
            ("TYCK-2", "chain", chain, [100, 200, 400, 800]),
            ("TYCK-3", "calls-same", calls_same, [64, 128, 256, 512]),
            ("TYCK-4", "ladder", ladder, [4, 8, 16, 32]),
            ("TYCK-5", "calls-imported", calls_imported, [16, 32, 64, 128]),
        ]
        for item, name, gen, xs in series:
            if b.quick:
                xs = xs[:2]
            dirs = {x: write(tmp, name, x, gen(x)) for x in xs}
            ys = b.sweep(name, xs, lambda x: cmd, cwd_of=lambda x: dirs[x])
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
