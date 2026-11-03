"""Long conditional sequences: a chain of n `&&` operands and a chain of n
guards, built and run in every pool language and in the nexus. Every
operand and every test is evaluated: the chain is decided by its last
element.

Build time is the wall time of `morloc make`, which is what is measured.
Run time in a pool is the per-call mean of the labeled chain, from the
nexus's benchmark summary (ANN), so pool start and loading are excluded.
A command the nexus evaluates itself cannot carry a label, so its run time
is the slope of the command's wall time against n."""

import os
import re
import shutil
import subprocess
import tempfile

from perf import fit_linear, growth_exponent

ROOTS = {"py": "root-py", "r": "root-r", "cpp": "root-cpp", "rs": "root-rust"}
LANGS = ["nexus", "py", "r", "cpp", "rs"]
AND_SIZES = [250, 500, 1000, 2000]
GUARD_SIZES = [100, 200, 400, 800]
CALLS = 200
LABELS = ('benchmark-template:\n  summary: "BENCH n={count} total={total} max={max}"\n'
          "labeled-groups:\n  b: { benchmark: true }\n")


def and_body(lang, n):
    if lang == "nexus":
        return "chain :: Bool -> Bool\nchain a = " + " && ".join(["a"] * n) + "\n"
    return "chain :: Int -> Bool\nchain x = " + "\n  && ".join(f"x >= {i}" for i in range(n)) + "\n"


def guard_body(lang, n):
    if lang == "nexus":
        arms = "".join(f"\n  ? b = {i}\n  :" for i in range(n - 1))
        return f"chain :: Bool -> Bool -> Int\nchain a b{arms} ? a = {n - 1}\n  : {n}\n"
    arms = "".join(f"\n  ? x == {i} = {i}\n  :" for i in range(n))
    return f"chain :: Int -> Int\nchain x{arms} {n}\n"


def program(shape, lang, n):
    body = (and_body if shape == "and" else guard_body)(lang, n)
    if lang == "nexus":
        sig = "Bool -> Bool" if shape == "and" else "Bool -> Bool -> Int"
        call = "g a = chain a" if shape == "and" else "g a b = chain a b"
        return f"module main (g)\n\n{body}\ng :: {sig}\n{call}\n"
    out = "Bool" if shape == "and" else "Int"
    return (f"module main (g)\n\nimport {ROOTS[lang]}\n\n{body}\n"
            f"g :: [Int] -> [{out}]\ng xs = map b@chain xs\n")


def args(lang, shape, n):
    """Inputs that make the last element decide the chain."""
    if lang == "nexus":
        return ["true"] if shape == "and" else ["true", "false"]
    x = n if shape == "and" else n - 1
    return ["[" + ",".join([str(x)] * CALLS) + "]"]


def expected(lang, shape, n):
    one = "true" if shape == "and" else str(n - 1)
    return one if lang == "nexus" else "[" + ",".join([one] * CALLS) + "]"


def call_cost(cmd, cwd, reps):
    """Per-call cost of the labeled chain, seconds, best over reps: the mean
    of every call but the slowest, and how much longer the slowest took.
    One call stands apart where the language compiles the function on a
    later call (R's JIT compiles a closure on its second call)."""
    steady, first = None, None
    for _ in range(reps):
        p = subprocess.run(cmd, cwd=cwd, capture_output=True, text=True)
        m = re.search(r"^BENCH n=(\d+) total=([0-9.e+-]+) max=([0-9.e+-]+)$", p.stderr, re.M)
        if p.returncode != 0 or not m or int(m.group(1)) != CALLS:
            raise RuntimeError(f"{cmd} in {cwd}: exit {p.returncode}, stderr {p.stderr[-500:]}")
        total, slowest = float(m.group(2)), float(m.group(3))
        s = (total - slowest) / (CALLS - 1)
        steady = s if steady is None else min(steady, s)
        first = slowest - s if first is None else min(first, slowest - s)
    return steady, first


def run(b):
    tmp = tempfile.mkdtemp(prefix="perf-chains-")
    try:
        for shape, sizes, item_build in [("and", AND_SIZES, "CHAIN-1"), ("guard", GUARD_SIZES, "CHAIN-2")]:
            if b.quick:
                sizes = sizes[:2]
            for lang in LANGS:
                name = f"{shape}-{lang}"
                dirs = {}
                for n in sizes:
                    d = dirs[n] = os.path.join(tmp, f"{name}-{n}")
                    os.makedirs(d)
                    with open(os.path.join(d, "main.loc"), "w") as h:
                        h.write(program(shape, lang, n))
                    if lang != "nexus":
                        with open(os.path.join(d, "main.yaml"), "w") as h:
                            h.write(LABELS)
                # Each repetition rebuilds in the same directory, so every
                # repetition does the same work.
                ys = b.sweep(f"{name}.make", sizes,
                             lambda n: ["morloc", "make", "-o", "nexus", "main.loc"],
                             reps=2, cwd_of=lambda n: dirs[n])
                c, slope, r2 = fit_linear(sizes, ys)
                b.record(item_build, f"{name}.make.b", slope * 1e3, "ms", f"r2={r2:.4f} per element")
                g = growth_exponent(sizes, ys)
                if g:
                    b.record(item_build, f"{name}.make.k", g[0], "", f"r2={g[1]:.4f}")
                for n in sizes:
                    out = subprocess.run(["./nexus", "g"] + args(lang, shape, n), cwd=dirs[n],
                                         capture_output=True, text=True)
                    if out.returncode != 0 or out.stdout.strip() != expected(lang, shape, n):
                        raise RuntimeError(f"{name} n={n}: exit {out.returncode}, "
                                           f"stdout {out.stdout.strip()[:200]!r}, stderr {out.stderr[-500:]}")
                b.record("CHAIN-3", f"{name}.run.max", sizes[-1], "elements", "largest chain run")
                if lang == "nexus":
                    ys = b.sweep(f"{name}.run", sizes, lambda n: ["./nexus", "g"] + args(lang, shape, n),
                                 cwd_of=lambda n: dirs[n])
                    note = "command wall time against n"
                else:
                    ys, firsts = [], []
                    for n in sizes:
                        t, w = call_cost(["./nexus", "g"] + args(lang, shape, n), dirs[n], b.reps)
                        ys.append(t)
                        firsts.append(w)
                        b.points.append({"series": f"{name}.call", "x": n, "best_s": t, "first_extra_s": w})
                        print(f"  {name + '.call':<24} x={n:<12} per call {t * 1e6:10.2f} us"
                              f"  slowest call +{w * 1e3:.2f} ms", flush=True)
                    note = f"labeled per-call mean, {CALLS} calls, slowest dropped"
                    _, wslope, wr2 = fit_linear(sizes, firsts)
                    b.record("CHAIN-5", f"{name}.first.b", wslope * 1e6, "us", f"r2={wr2:.4f} per element")
                    g = growth_exponent(sizes, firsts)
                    if g:
                        b.record("CHAIN-5", f"{name}.first.k", g[0], "", f"r2={g[1]:.4f}")
                _, slope, r2 = fit_linear(sizes, ys)
                b.record("CHAIN-4", f"{name}.run.b", slope * 1e9, "ns", f"r2={r2:.4f} per element; {note}")
    finally:
        shutil.rmtree(tmp)
