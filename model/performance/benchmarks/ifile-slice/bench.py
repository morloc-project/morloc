"""IFile slices read in place (ENC-9, ENC-10). A file of n Reals (or Strs)
is read whole with one `.[0:n]` and with an IStream `@next` loop, in C++
and Rust, uncompressed and at level 3; the ratio of the per-element slopes
is the slice's cost against the stream's. Then k one-element slices are
timed on files of growing size: their cost per slice must not grow with
the file.

ENC-13: strided (2, 7), reversed and field-tail slices against a unit slice
with the same output count, in C++, Rust and Python, uncompressed and at
level 3, over [Real], a tuple (I64, Real, Real) and [Str]. Each pair records
its time ratio and the ratio of their peak RSS."""

import os
import shutil
import tempfile

RUST_ENV = {"MORLOC_LANG_PARAMS": "rust:lto=off"}
REAL_SIZES = [0, 4000000, 8000000, 16000000]
STR_SIZES = [0, 1000000, 2000000, 4000000]
POINT_FILES = [1000000, 4000000, 16000000]
POINTS = 10000
LANGS = {"cpp": ("cpp.loc", None), "rs": ("rs.loc", RUST_ENV)}
STRIDE_LANGS = {"cpp": ("cpp.loc", None), "rs": ("rs.loc", RUST_ENV), "py": ("py.loc", None)}
STRIDE_REALS = 16000000
STRIDE_TUPS = 5000000
STRIDE_STRS = 4000000


def run(b):
    for lang, (loc, env) in STRIDE_LANGS.items():
        b.make(loc=loc, out=f"nexus-{lang}", env=env)
    real_ns = [0, 1000000] if b.quick else REAL_SIZES
    str_ns = [0, 250000] if b.quick else STR_SIZES
    point_ns = [1000000, 2000000] if b.quick else POINT_FILES
    tmp = tempfile.mkdtemp(prefix="perf-ifs-")
    try:
        for lang in LANGS:
            exe = f"./nexus-{lang}"
            for z in (0, 3):
                for kind, ns in (("Reals", real_ns), ("Strs", str_ns)):
                    path = lambda n: os.path.join(tmp, f"{lang}-{kind}-{z}-{n}.mlc")
                    for n in ns:
                        b.time([exe, "write" + kind, str(z), str(n), path(n)], reps=1)
                    series = f"{lang}.{kind}.z{z}"
                    _, bs, _ = b.sweep_linear("ENC-9", f"{series}.slice", ns,
                                              lambda n: [exe, "slice" + kind, str(n), path(n)],
                                              "element", b_unit="ns")
                    _, bn, _ = b.sweep_linear("ENC-9", f"{series}.next", ns,
                                              lambda n: [exe, "next" + kind, path(n)],
                                              "element", b_unit="ns")
                    if bn > 0:
                        b.record("ENC-9", f"{series}.ratio", bs / bn, "x",
                                 "slice slope over @next slope")
                    for n in ns:
                        os.remove(path(n))
                path = lambda n: os.path.join(tmp, f"{lang}-points-{z}-{n}.mlc")
                per = []
                for n in point_ns:
                    b.time([exe, "writeReals", str(z), str(n), path(n)], reps=1)
                    base = b.time([exe, "pointsReals", "0", path(n)])
                    t = b.time([exe, "pointsReals", str(POINTS), path(n)])
                    b.point(f"{lang}.points.z{z}", n, t)
                    per.append((t.best - base.best) / POINTS)
                    b.record("ENC-10", f"{lang}.points.z{z}.n{n}", per[-1] * 1e6, "us", "per one-element slice")
                    os.remove(path(n))
                if per[0] > 0:
                    b.record("ENC-10", f"{lang}.points.z{z}.growth", per[-1] / per[0], "x",
                             f"per-slice cost at n={point_ns[-1]} over n={point_ns[0]}")
        stride(b, tmp)
    finally:
        shutil.rmtree(tmp)


def stride(b, tmp):
    nr = 1000000 if b.quick else STRIDE_REALS
    nt = 400000 if b.quick else STRIDE_TUPS
    ns = 250000 if b.quick else STRIDE_STRS
    for lang in STRIDE_LANGS:
        exe = f"./nexus-{lang}"
        for z in (0, 3):
            reals = os.path.join(tmp, f"s-{lang}-reals-{z}.mlc")
            tups = os.path.join(tmp, f"s-{lang}-tups-{z}.mlc")
            strs = os.path.join(tmp, f"s-{lang}-strs-{z}.mlc")
            b.time([exe, "writeReals", str(z), str(nr), reals], reps=1)
            b.time([exe, "writeTups", str(z), str(nt), tups], reps=1)
            b.time([exe, "writeStrs", str(z), str(ns), strs], reps=1)
            pairs = [
                ("reals.k2", [exe, "strideReals", str(nr), "2", reals], [exe, "sliceReals", str(nr // 2), reals], 8 * (nr // 2)),
                ("reals.k7", [exe, "strideReals", str(nr), "7", reals],
                 [exe, "sliceReals", str(-(-nr // 7)), reals], 8 * -(-nr // 7)),
                ("reals.rev", [exe, "revReals", reals], [exe, "sliceReals", str(nr), reals], 8 * nr),
                ("tups.k2", [exe, "strideTups", str(nt), "2", tups], [exe, "strideTups", str(nt // 2), "1", tups], 24 * (nt // 2)),
                ("tups.field", [exe, "fieldTups", str(nt), tups], [exe, "sliceReals", str(nt), reals], 8 * nt),
                ("strs.k2", [exe, "strideStrs", str(ns), "2", strs], [exe, "sliceStrs", str(ns // 2), strs], 36 * (ns // 2)),
            ]
            for name, strided, unit, out_bytes in pairs:
                series = f"{lang}.{name}.z{z}"
                ts = b.time(strided)
                tu = b.time(unit)
                b.point(f"{series}.strided", out_bytes, ts)
                b.point(f"{series}.unit", out_bytes, tu)
                if tu.best > 0:
                    b.record("ENC-13", f"{series}.ratio", ts.best / tu.best, "x", "strided over unit, same output count")
                if tu.maxrss_kib > 0:
                    b.record("ENC-13", f"{series}.rssx", ts.maxrss_kib / tu.maxrss_kib, "x",
                             f"peak RSS over the unit slice's; output about {out_bytes / 2**20:.0f} MiB")
            for f in (reals, tups, strs):
                os.remove(f)
