"""Stream compression: ratio and time by corpus, level, worker count, size
relative to the 16 MiB frame, and batching. See compression.md for the
experiments (E1-E5) and the items they feed."""

import math
import os
import shutil
import tempfile

from perf import fit_linear, fit_linear2

MiB = 1 << 20
FRAME = 16 * MiB

# name: (write/read command suffix, kind)
CORPORA = {
    "u64-uniform": ("U64", 0),
    "u64-small": ("U64", 1),
    "u64-sorted": ("U64", 2),
    "u64-zero": ("U64", 3),
    "real-walk": ("Real", 0),
    "str-dna": ("Str", 0),
    "str-fastq": ("Str", 1),
}
# morloc level -> (zstd level, long mode); compression.rs `zstd_level`.
ZSTD = {1: (1, False), 2: (3, False), 3: (6, False), 4: (9, False), 5: (12, False),
        6: (15, False), 7: (19, True), 8: (21, True), 9: (22, True)}
WORKERS = [1, 2, 3, 4, 6, 8, 12]


class Ctx:
    def __init__(self, b, tmp):
        self.b = b
        self.tmp = tmp
        self.bpe = {}

    def path(self, *parts):
        return os.path.join(self.tmp, "-".join(str(p) for p in parts) + ".mlc")

    def write_cmd(self, corpus, lvl, n, path, batch=0, workers=None, cores=None):
        t, kind = CORPORA[corpus]
        cmd = ["./nexus", "write" + t, str(lvl), str(kind), str(n), str(batch), path]
        if workers:
            cmd = ["env", f"MORLOC_FRAME_WORKERS={workers}"] + cmd
        if cores:
            cmd = ["taskset", "-c", f"0-{cores - 1}"] + cmd
        return cmd

    def read_cmd(self, corpus, n, path, workers=None):
        t, _ = CORPORA[corpus]
        cmd = ["./nexus", "read" + t, str(n), path]
        if workers:
            cmd = ["env", f"MORLOC_FRAME_WORKERS={workers}"] + cmd
        return cmd

    def elements(self, corpus, nbytes):
        """Elements of `corpus` whose level-0 stream file is about nbytes."""
        if corpus not in self.bpe:
            n = 1 << 20
            p = self.path("probe", corpus)
            self.b.time(self.write_cmd(corpus, 0, n, p), reps=1)
            self.bpe[corpus] = os.path.getsize(p) / n
            os.remove(p)
        return max(1, int(nbytes / self.bpe[corpus]))


def zstd_baseline(c, corpus, raw_path, lvl, long_mode, mb):
    zl, _ = ZSTD[lvl]
    out = raw_path + ".zst"
    cmd = ["zstd", "-q", "-f", "-T1", f"-{zl}", raw_path, "-o", out]
    if zl > 19:
        cmd.insert(1, "--ultra")
    if long_mode:
        cmd.insert(1, "--long=27")
    t = c.b.time(cmd, reps=1 if lvl >= 7 else 2)
    ratio = os.path.getsize(out) / os.path.getsize(raw_path)
    os.remove(out)
    tag = "zstd-long" if long_mode else "zstd"
    c.b.point(f"E1:{corpus}:{tag}", lvl, t)
    return t.best / mb, ratio


def e1_levels(c):
    """Every level, every corpus, one worker, two frames."""
    b = c.b
    for corpus in CORPORA:
        n = c.elements(corpus, 2 * FRAME)
        levels = [0, 1, 3] if b.quick else list(range(10))
        paths = {lvl: c.path("e1", corpus, lvl) for lvl in levels}
        wt = dict(zip(levels, b.sweep(f"E1:{corpus}:write", levels,
                                      lambda l: c.write_cmd(corpus, l, n, paths[l], workers=1),
                                      reps=2)))
        rt = dict(zip(levels, b.sweep(f"E1:{corpus}:read", levels,
                                      lambda l: c.read_cmd(corpus, n, paths[l], workers=1),
                                      reps=2)))
        raw = os.path.getsize(paths[0])
        mb = raw / 1e6
        b.record("ZIP-1", f"{corpus}.z0", wt[0] / mb * 1e3, "ms/MB",
                 "level 0 write: generation, encoding and file write")
        for lvl in levels[1:]:
            size = os.path.getsize(paths[lvl])
            b.record("ZIP-1", f"{corpus}.z{lvl}", (wt[lvl] - wt[0]) / mb * 1e3, "ms/MB",
                     f"over level 0, one worker; {mb / max(wt[lvl] - wt[0], 1e-9):.0f} MB/s")
            b.record("ZIP-2", f"{corpus}.z{lvl}", (rt[lvl] - rt[0]) / mb * 1e3, "ms/MB",
                     "over level 0, one worker")
            b.record("ZIP-3", f"{corpus}.z{lvl}", size / raw, "x", "file / level-0 file")
            zt, zr = zstd_baseline(c, corpus, paths[0], lvl, False, mb)
            b.record("ZIP-4", f"{corpus}.z{lvl}.time", (wt[lvl] - wt[0]) / mb / zt, "x",
                     f"morloc / zstd -T1 -{ZSTD[lvl][0]} per MB")
            b.record("ZIP-4", f"{corpus}.z{lvl}.ratio", (size / raw) / zr, "x",
                     "morloc ratio / zstd single-frame ratio")
            if ZSTD[lvl][1]:
                _, zlr = zstd_baseline(c, corpus, paths[0], lvl, True, mb)
                b.record("ZIP-4", f"{corpus}.z{lvl}.long", zlr / zr, "x",
                         "zstd ratio with --long=27 / without, single frame")
        for p in paths.values():
            os.remove(p)


def e2_workers(c):
    """Worker count against time, at eight frames."""
    b = c.b
    corpora = ["str-fastq"] if b.quick else ["str-fastq", "u64-small"]
    levels = [3] if b.quick else [1, 3, 6]
    ws = [1, 2] if b.quick else WORKERS
    for corpus in corpora:
        n = c.elements(corpus, 8 * FRAME)
        for lvl in levels:
            paths = {w: c.path("e2", corpus, lvl, w) for w in ws}
            wt = b.sweep(f"E2:{corpus}:z{lvl}:write", ws,
                         lambda w: c.write_cmd(corpus, lvl, n, paths[w], workers=w), reps=3)
            rt = b.sweep(f"E2:{corpus}:z{lvl}:read", ws,
                         lambda w: c.read_cmd(corpus, n, paths[w], workers=w), reps=3)
            for name, ts in (("write", wt), ("read", rt)):
                phys = [(w, t) for w, t in zip(ws, ts) if w <= 6]
                a, B, r2 = fit_linear([1.0 / w for w, _ in phys], [t for _, t in phys])
                b.record("ZIP-5", f"{corpus}.z{lvl}.{name}.a", a * 1e3, "ms", f"r2={r2:.4f}")
                b.record("ZIP-5", f"{corpus}.z{lvl}.{name}.B", B * 1e3, "ms", f"r2={r2:.4f}")
                for w, t in zip(ws, ts):
                    if w in (6, 12):
                        b.record("ZIP-5", f"{corpus}.z{lvl}.{name}.speedup{w}", ts[0] / t, "x")
            for p in paths.values():
                os.remove(p)
        # The default worker count follows the CPU affinity mask.
        if not b.quick:
            cores = [1, 2, 4, 6]
            paths = {k: c.path("e2t", corpus, k) for k in cores}
            tt = b.sweep(f"E2:{corpus}:z3:taskset", cores,
                         lambda k: c.write_cmd(corpus, 3, n, paths[k], cores=k), reps=3)
            ref = dict(zip(cores, b.sweep(f"E2:{corpus}:z3:workers-ref", cores,
                                       lambda k: c.write_cmd(corpus, 3, n, paths[k], workers=k),
                                       reps=3)))
            for k, t in zip(cores, tt):
                b.record("ZIP-5", f"{corpus}.taskset{k}", t / ref[k], "x",
                         f"taskset -c 0-{k - 1} / MORLOC_FRAME_WORKERS={k}")
            for p in paths.values():
                os.remove(p)


def e3_edges(c):
    """Size around frame boundaries, one worker and all workers."""
    b = c.b
    corpus = "str-fastq"
    frames = [0.25, 1, 1.0625] if b.quick else \
        [0.0625, 0.25, 0.5, 1, 1.0625, 2, 2.0625, 4, 4.0625, 8]
    ns = {f: c.elements(corpus, f * FRAME) for f in frames}
    for workers in (1, None):
        tag = "w1" if workers else "wall"
        paths = {f: c.path("e3", tag, f) for f in frames}
        ts = b.sweep(f"E3:{tag}:write", frames,
                     lambda f: c.write_cmd(corpus, 3, ns[f], paths[f], workers=workers), reps=3)
        raw = [ns[f] * c.bpe[corpus] for f in frames]
        out = [os.path.getsize(paths[f]) for f in frames]
        k = [math.ceil(r / FRAME) for r in raw]
        if workers:
            cst, r, h, r2 = fit_linear2(raw, k, out)
            b.record("ZIP-6", "fastq.z3.r", r, "x", f"r2={r2:.5f}, bytes out per raw byte")
            b.record("ZIP-6", "fastq.z3.h", h / 1024, "KiB", f"r2={r2:.5f}, extra bytes per frame")
            ct, bt, r2t = fit_linear([x / 1e6 for x in raw], ts)
            b.record("ZIP-6", "fastq.z3.w1.b", bt * 1e3, "ms/MB", f"r2={r2t:.4f}")
        else:
            # With a worker per frame, wall time follows the largest frame,
            # min(m, F), rather than m.
            ct, bt, r2t = fit_linear([min(x, FRAME) / 1e6 for x in raw], ts)
            b.record("ZIP-6", "fastq.z3.wall.b", bt * 1e3, "ms/MB",
                     f"r2={r2t:.4f}, against min(m, 16 MiB)")
        for p in paths.values():
            os.remove(p)


def e4_batches(c):
    """Batch size with @flush per batch, at four frames."""
    b = c.b
    corpus = "str-fastq"
    m = 4 * FRAME
    n = c.elements(corpus, m)
    sizes = [MiB, 0] if b.quick else [64 << 10, 256 << 10, MiB, 4 * MiB, 16 * MiB, 0]
    batch = {s: (c.elements(corpus, s) if s else 0) for s in sizes}
    paths = {s: c.path("e4", s) for s in sizes}
    ts = b.sweep("E4:write", sizes,
                 lambda s: c.write_cmd(corpus, 3, n, paths[s], batch=batch[s]), reps=3)
    flushed = [(s, t) for s, t in zip(sizes, ts) if s]
    k = [math.ceil(n / batch[s]) for s, _ in flushed]
    out = [os.path.getsize(paths[s]) for s, _ in flushed]
    cst, h, r2 = fit_linear(k, out)
    b.record("ZIP-7", "fastq.z3.h", h / 1024, "KiB", f"r2={r2:.4f}, extra bytes per flushed batch")
    ct, d, r2t = fit_linear(k, [t for _, t in flushed])
    b.record("ZIP-7", "fastq.z3.d", d * 1e3, "ms", f"r2={r2t:.4f}, time per flushed batch")
    whole = os.path.getsize(paths[0])
    for s, o in zip([s for s, _ in flushed], out):
        b.record("ZIP-7", f"fastq.z3.batch{s >> 10}k", o / whole, "x", "file / one @write")
    for p in paths.values():
        os.remove(p)


def run(b):
    b.make()
    tmp = tempfile.mkdtemp(prefix="perf-zip-")
    try:
        c = Ctx(b, tmp)
        e1_levels(c)
        e2_workers(c)
        e3_edges(c)
        e4_batches(c)
    finally:
        shutil.rmtree(tmp)
