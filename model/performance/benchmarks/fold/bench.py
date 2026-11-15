"""A folding @render over b batches of 64 keys (ENC-14): a scalar count and
a key-count Map with k distinct keys, in C++ and Python, against a control
that does the same counting where the batches are made. The fold's own
cost is the slope of the fold over the control's, per batch.
"""

import perf

S = 64
SERIES = {
    # name: (fold command, control command, keys, batch counts)
    "cpp.n": ("foldNCpp", "controlNCpp", 100, [0, 1000, 4000, 10000]),
    "cpp.map1k": ("foldMapCpp", "controlMapCpp", 1000, [0, 500, 2000, 5000]),
    "cpp.map10k": ("foldMapCpp", "controlMapCpp", 10000, [0, 500, 2000, 5000]),
    "py.map1k": ("foldMapPy", "controlMapPy", 1000, [0, 200, 800, 2000]),
    "py.map10k": ("foldMapPy", "controlMapPy", 10000, [0, 200, 800, 2000]),
}
QUICK = [0, 100]


def slope(b, series, xs):
    rows = {p["x"]: p for p in b.points if p["series"] == series}
    _, bw, r2 = perf.fit_linear(xs, [rows[x]["best_s"] for x in xs])
    return bw, r2


def run(b):
    b.make()
    for name, (fold, control, k, nbs) in SERIES.items():
        xs = QUICK if b.quick else nbs
        b.sweep(f"{name}.fold", xs, lambda x, f=fold, k=k: ["./nexus", f, "--n", str(k), str(S), str(x)])
        b.sweep(f"{name}.control", xs, lambda x, c=control, k=k: ["./nexus", c, str(k), str(S), str(x)])
        bf, r2f = slope(b, f"{name}.fold", xs)
        bc, r2c = slope(b, f"{name}.control", xs)
        b.record("ENC-14", f"{name}.fold.b", bf * 1e6, "us/batch", f"r2={r2f:.4f}")
        b.record("ENC-14", f"{name}.control.b", bc * 1e6, "us/batch", f"r2={r2c:.4f}")
        b.record("ENC-14", f"{name}.extra", (bf - bc) * 1e6, "us/batch", "fold slope over the control's")
