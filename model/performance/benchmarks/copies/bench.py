"""Copies: wall time of copy-heavy shapes in generated Rust and C++, with the
copies each makes per step. Every program prints [result, copies]; a series
is timed at one size and at a control size that does the same setup with
fewer or no steps, so the difference is the cost of the steps alone."""

import json

RUST_ENV = {"MORLOC_LANG_PARAMS": "rust:lto=off"}

# (item, series, program, export, args, steps, control args, control steps)
SERIES = [
    ("COPY-1", "rs.nestedLoop", "nexus-rs", "nestedLoop", [4000], 2000, [2], 1),
    ("COPY-1", "cpp.nestedLoop", "nexus-cpp", "nestedLoop", [10000, 100], 500000, [0, 100], 0),
    ("COPY-2", "rs.tlLoop", "nexus-rs", "tlLoop", [1000, 10000], 1000, [0, 10000], 0),
    ("COPY-3", "rs.listLoop", "nexus-rs", "listLoop", [1000, 5000], 1000, [0, 5000], 0),
    ("COPY-3", "cpp.listLoop", "nexus-cpp", "listLoop", [250000], 250000, [0], 0),
    ("COPY-4", "rs.closure", "nexus-rs", "closure", [1000, 10000], 1000, [0, 10000], 0),
    ("COPY-4", "cpp.closure", "nexus-cpp", "closure", [500000], 500000, [0], 0),
    ("COPY-5", "cpp.tupleStep", "nexus-cpp", "tupleStep", [250000], 250000, [0], 0),
]

QUICK_SCALE = 10


def run(b):
    b.make(loc="main-rs.loc", out="nexus-rs", env=RUST_ENV)
    b.make(loc="main-cpp.loc", out="nexus-cpp")
    for item, series, prog, export, args, steps, cargs, csteps in SERIES:
        if b.quick:
            args = [max(1, args[0] // QUICK_SCALE)] + args[1:]
            steps = max(1, steps // QUICK_SCALE)
        cmd = [f"./{prog}", export] + [str(a) for a in args]
        ctl = [f"./{prog}", export] + [str(a) for a in cargs]
        big = b.time(cmd)
        small = b.time(ctl)
        copies = json.loads(big.stdout)[1]
        ccopies = json.loads(small.stdout)[1]
        b.point(series, " ".join(map(str, args)), big, copies=copies, steps=steps)
        b.point(series + ".ctl", " ".join(map(str, cargs)), small, copies=ccopies, steps=csteps)
        net = big.best - small.best
        dcopies = copies - ccopies
        note = f"args {args}, {dcopies} copies over {steps - csteps} steps"
        b.record(item, f"{series}.t", big.best, "s", note)
        b.record(item, f"{series}.net", net, "s", note)
        b.record(item, f"{series}.per_step", dcopies / (steps - csteps), "copies", note)
        if dcopies > 0:
            b.record(item, f"{series}.u", net / dcopies * 1e9, "ns", "net time per copy")
