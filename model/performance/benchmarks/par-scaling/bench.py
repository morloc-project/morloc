from perf import fit_linear

# Iterations of `spin` per element, chosen so the sequential run of N
# elements takes about two seconds.
BASE = {"cpp": 10000000, "py": 100000, "rs": 10000000}
N = 200
WORKERS = [1, 2, 4, 8]
# Elements of zero cost, for the per-element overhead of dispatch.
OVERHEAD_N = [0, 50000, 100000, 200000]


def run(b):
    for lang in BASE:
        b.make(loc=f"{lang}.loc", out=lang)
    for lang, base in BASE.items():
        ws = WORKERS[:2] if b.quick else WORKERS
        # x = 0 is the sequential `map`; otherwise parMap with x workers,
        # shrinking chunks (parMap workers chunking chunk shape n base).
        cmd = {0: [f"./{lang}", "seqMap", "0", str(N), str(base)]}
        for w in ws:
            cmd[w] = [f"./{lang}", "parMap", str(w), "1", "0", "0", str(N), str(base)]
        seq, *ts = b.sweep(f"{lang}-workers", [0] + ws, lambda w: cmd[w])
        # t(w) = a + B/w: `a` is the part no worker count shortens.
        a, B, r2 = fit_linear([1.0 / w for w in ws], ts)
        b.record("PAR-1", f"{lang}.a", a * 1e3, "ms", f"r2={r2:.4f}")
        b.record("PAR-1", f"{lang}.B", B * 1e3, "ms", f"r2={r2:.4f}")
        b.record("PAR-1", f"{lang}.speedup{ws[-1]}", seq / ts[-1], "x",
                 f"seq {seq:.3f}s, {ws[-1]} workers {ts[-1]:.3f}s")
        ns = OVERHEAD_N[:2] if b.quick else OVERHEAD_N
        _, bs, _ = b.sweep_linear("PAR-2", f"{lang}-seq0", ns,
                                  lambda n: [f"./{lang}", "seqMap", "0", str(n), "0"],
                                  "element", b_unit="ns")
        _, bp, _ = b.sweep_linear("PAR-2", f"{lang}-par0", ns,
                                  lambda n: [f"./{lang}", "parMap", "4", "1", "0", "0", str(n), "0"],
                                  "element", b_unit="ns")
        b.record("PAR-2", f"{lang}.overhead", (bp - bs) * 1e9, "ns",
                 "per element, 4 workers, minus sequential")
