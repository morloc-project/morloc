"""A whole-list @collect (ENC-11). b batches of k 256-count U64 histograms
(2 KiB each) are gathered whole into a handler that summarizes the list,
and the same batches are made and counted where they are made with no
gather (the control). The gather's own cost is the slope of the gather
over the control's, per GiB gathered. Peak memory is sampled over the
whole process tree.
"""

import os
import shutil
import subprocess

import perf

GIB = 1024 ** 3
HIST = 256 * 8
SERIES = {
    # name: (gather command, control command, k, batch counts)
    "cpp": ("gatherCpp", "controlCpp", 1024, [0, 32, 128, 320]),
    "py": ("gatherPy", "controlPy", 256, [0, 16, 64, 128]),
}
QUICK = {"cpp": [0, 16], "py": [0, 8]}


def gib(k, nb):
    return k * nb * HIST / GIB


def slope_per_gib(b, series, k, xs):
    """Fit wall and wall - CPU of the newest points of `series` against GiB.
    Only the wall slope is trusted: pools are not waited-for children, so
    the CPU column misses their time."""
    rows = {p["x"]: p for p in b.points if p["series"] == series}
    gs = [gib(k, x) for x in xs]
    walls = [rows[x]["best_s"] for x in xs]
    off = [rows[x]["best_s"] - float(rows[x].get("cpu_s", 0)) for x in xs]
    _, bw, r2 = perf.fit_linear(gs, walls)
    _, bo, r2o = perf.fit_linear(gs, off)
    return bw, r2, bo, r2o


def run(b):
    perf.OPTIONS["tree_mem"] = True
    perf.OPTIONS["sync"] = True
    b.make()
    shim = b.path("nexus-build", "old_load.so")
    subprocess.run(["cc", "-O2", "-shared", "-fPIC", "-o", shim, b.path("old_load.c")], check=True)
    for name, (gather, control, k, nbs) in SERIES.items():
        nbs = QUICK[name] if b.quick else nbs
        xs = nbs
        b.sweep(f"{name}.gather.k{k}", xs,
                lambda x, g=gather, k=k: ["./nexus", g, "--sum", str(k), str(x)])
        b.sweep(f"{name}.control.k{k}", xs,
                lambda x, c=control, k=k: ["./nexus", c, str(k), str(x)])
        bg, r2g, _, _ = slope_per_gib(b, f"{name}.gather.k{k}", k, xs)
        bc, r2c, _, _ = slope_per_gib(b, f"{name}.control.k{k}", k, xs)
        b.record("ENC-11", f"{name}.gather.b", bg, "s/GiB", f"r2={r2g:.4f}")
        b.record("ENC-11", f"{name}.control.b", bc, "s/GiB", f"r2={r2c:.4f}")
        b.record("ENC-11", f"{name}.extra", bg - bc, "s/GiB", "gather slope over the control's")
        # The halves: write the batches to a file, then load it whole. The
        # file is written once per x, outside the timed load.
        lang = "Cpp" if name == "cpp" else "Py"
        stage = b.path(f"stage-{name}")
        os.makedirs(stage, exist_ok=True)
        fpath = lambda x: os.path.join(stage, f"{k}-{x}.st")
        b.sweep(f"{name}.write.k{k}", xs,
                lambda x, lang=lang, k=k: ["./nexus", "write" + lang, str(k), str(x), fpath(x)])
        b.sweep(f"{name}.load.k{k}", xs,
                lambda x, lang=lang: ["./nexus", "load" + lang, fpath(x)])
        bw, r2w, _, _ = slope_per_gib(b, f"{name}.write.k{k}", k, xs)
        bl, r2l, _, _ = slope_per_gib(b, f"{name}.load.k{k}", k, xs)
        b.record("ENC-11", f"{name}.write.b", bw - bc, "s/GiB", f"write slope over the control's, r2={r2w:.4f}")
        b.record("ENC-11", f"{name}.load.b", bl, "s/GiB", f"whole load slope, r2={r2l:.4f}")
        shutil.rmtree(stage)

    # W2's in-run control: the same build with IFile opens refused, so the
    # gather's @load reads the file into one block as before.
    k, nbs = 1024, ([0, 16] if b.quick else [0, 32, 128, 320])
    walls = b.sweep_variants("cpp.load", {"inplace": {}, "block": {"LD_PRELOAD": shim}}, nbs,
                             lambda x: ["./nexus", "gatherCpp", "--sum", str(k), str(x)])
    gs = [gib(k, x) for x in nbs]
    _, bi, _ = perf.fit_linear(gs, walls["inplace"])
    _, bb, _ = perf.fit_linear(gs, walls["block"])
    b.record("ENC-11", "cpp.load.inplace.b", bi, "s/GiB", "gather slope, @load decoded in place")
    b.record("ENC-11", "cpp.load.block.b", bb, "s/GiB", "gather slope, @load into one block (control)")

    # The same gather with the action writing to a path, which takes
    # another route through the nexus.
    b.sweep("cpp.topath.k1024", nbs,
            lambda x: ["./nexus", "gatherCpp", "--no-stdout", "--sum=/dev/null", str(k), str(x)])
    bp, r2p, _, _ = slope_per_gib(b, "cpp.topath.k1024", k, nbs)
    b.record("ENC-11", "cpp.topath.b", bp, "s/GiB", f"gather slope, action written to a path, r2={r2p:.4f}")
