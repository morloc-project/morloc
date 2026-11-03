import os
import shutil
import tempfile

from perf import fit_linear, growth_exponent, timed

CMDS = ["py", "cpp", "r", "rs", "pyCpp", "all4"]


def run(b):
    b.make()
    reps = 3 if b.quick else 15
    cmd = {c: ["./nexus", c, "1"] for c in CMDS}
    cmd["help"] = ["./nexus", "-h"]
    names = ["help"] + CMDS
    best = dict(zip(names, b.sweep("start", names, lambda c: cmd[c], reps=reps)))
    b.record("START-1", "help", best["help"] * 1e3, "ms", "nexus alone, no pool")
    for c in CMDS:
        b.record("START-2", f"{c}.wall", best[c] * 1e3, "ms")
    singles = [best[c] for c in ("py", "cpp", "r", "rs")]
    b.record("START-3", "all4.wall", best["all4"] * 1e3, "ms",
             f"sum of single-pool starts {sum(singles) * 1e3:.0f} ms, "
             f"largest {max(singles) * 1e3:.0f} ms")

    # START-4: a pool's start against the length of its longest function,
    # a run of n lets, which every pool writes as one function.
    tmp = tempfile.mkdtemp(prefix="perf-start-")
    try:
        sizes = [500, 1000] if b.quick else [500, 1000, 2000, 4000]
        for lang, root in [("py", "root-py"), ("r", "root-r")]:
            dirs = {}
            for n in sizes:
                d = dirs[n] = os.path.join(tmp, f"{lang}-{n}")
                os.makedirs(d)
                lets = "".join(f"\n      y{i} = y{i - 1} + 1" for i in range(1, n))
                with open(os.path.join(d, "main.loc"), "w") as h:
                    h.write(f"module main (f)\n\nimport {root}\n\nf :: Int -> Int\n"
                            f"f x =\n  let y0 = x{lets}\n  in y{n - 1}\n")
                timed(["morloc", "make", "-o", "nexus", "main.loc"], d)
            ys = b.sweep(f"start-{lang}-lets", sizes, lambda n: ["./nexus", "f", "1"],
                         reps=reps, cwd_of=lambda n: dirs[n])
            c, slope, r2 = fit_linear(sizes, ys)
            b.record("START-4", f"{lang}.b", slope * 1e6, "us", f"r2={r2:.4f} per let")
            g = growth_exponent(sizes, ys)
            if g:
                b.record("START-4", f"{lang}.k", g[0], "", f"r2={g[1]:.4f}")
    finally:
        shutil.rmtree(tmp)
