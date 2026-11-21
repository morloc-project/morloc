"""COPY-7: b batches of S integers folded into one growing list. The time
grows as b**p; p near 2 means each step copies the accumulator, near 1
that it moves it. Rust's @fold with a by-value step (val.loc), with a
by-reference step, the Rust stdlib fold, and C++'s @fold for reference.
"""

import perf

S = 1000
# A series that copies the accumulator is quadratic, so its counts stay
# small; a series that moves it runs to larger counts.
COUNTS = [250, 500, 1000, 2000]
WIDE = [1000, 2000, 4000, 8000]
QUICK = [250, 500, 1000]
SERIES = {
    # name: (nexus, command, argv tail, batch counts)
    "rs.fold.val": ("nexus-val", "foldValRs", ["--n"], WIDE),
    "cpp.fold.val": ("nexus", "foldValCpp", ["--n"], WIDE),
    "rs.fold.ref": ("nexus", "foldRefRs", ["--n"], COUNTS),
    "rs.hof.ref": ("nexus", "hofRefRs", [], COUNTS),
}


def run(b):
    b.make()
    b.make(loc="val.loc", out="nexus-val")
    for name, (nexus, cmd, flags, counts) in SERIES.items():
        xs = QUICK if b.quick else counts
        b.sweep(name, xs, lambda x, n=nexus, c=cmd, f=flags: [f"./{n}", c, *f, str(S), str(x)])
        rows = {p["x"]: p for p in b.points if p["series"] == name}
        fit = perf.growth_exponent(xs, [rows[x]["best_s"] for x in xs])
        if fit is None:
            b.record("COPY-7", f"{name}.p", float("nan"), "", "time did not grow")
        else:
            b.record("COPY-7", f"{name}.p", fit[0], "", f"time ~ b**p, r2={fit[1]:.3f}")
