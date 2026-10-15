import os
import subprocess
import tempfile
import time

from perf import fit_linear

SIZES = [0, 250000, 500000, 1000000]
HEADER = "name,chrom,start,end,score,keep\n"

# (series, nexus command, headed input, standard mode)
CASES = [
    ("table:Table", "tableTable", True, "table"),
    ("default:Table", "tableDefault", True, "table"),
    ("table:Records", "recsTable", True, "records"),
    ("list:Records", "recsList", False, "records"),
    ("list:Tuples", "tupsList", False, "records"),
]


def write_rows(path, n, headed):
    with open(path, "w") as f:
        if headed:
            f.write(HEADER)
        for i in range(n):
            f.write(f"gene{i:07d},chr{i % 23 + 1},{i * 10},{i * 10 + 7},"
                    f"{i * 0.25},{'true' if i % 2 else 'false'}\n")


def descendants(pid):
    out, todo = [], [pid]
    while todo:
        p = todo.pop()
        out.append(p)
        try:
            for t in os.listdir(f"/proc/{p}/task"):
                with open(f"/proc/{p}/task/{t}/children") as f:
                    todo.extend(int(c) for c in f.read().split())
        except OSError:
            pass
    return out


def hwm_kib(pid):
    try:
        with open(f"/proc/{pid}/status") as f:
            for line in f:
                if line.startswith("VmHWM:"):
                    return int(line.split()[1])
    except OSError:
        pass
    return 0


def tree_peak(cmd, cwd):
    """Run cmd once, polling every process in its tree for VmHWM. Returns
    (sum over processes of their peaks, the largest one), in KiB. The sum
    bounds the concurrent peak from above; shared memory touched by two
    processes counts in both."""
    p = subprocess.Popen(cmd, cwd=cwd, stdout=subprocess.DEVNULL,
                         stderr=subprocess.PIPE)
    peaks = {}
    while p.poll() is None:
        for q in descendants(p.pid):
            h = hwm_kib(q)
            if h > peaks.get(q, 0):
                peaks[q] = h
        time.sleep(0.005)
    err = p.stderr.read()
    if p.returncode != 0:
        raise RuntimeError(f"{cmd} exited {p.returncode}: {err.decode(errors='replace')}")
    return sum(peaks.values()), max(peaks.values(), default=0)


def run(b):
    b.make()
    tmp = tempfile.mkdtemp(prefix="perf-tbl-")
    ns = [0, 100000] if b.quick else SIZES
    big = ns[-1]
    std = os.path.join(tmp, "standard")
    subprocess.run(["rustc", "-O", "-o", std, b.path("standard.rs")], check=True)
    files = {}
    try:
        for n in ns:
            for headed in (True, False):
                p = os.path.join(tmp, f"{n}.{'h' if headed else 'n'}.csv")
                write_rows(p, n, headed)
                files[(n, headed)] = p
        b.record("TBL-1", "input.bytes_per_row",
                 os.path.getsize(files[(big, True)]) / max(big, 1), "B")

        # The size of the loaded value: its morloc packet.
        sizes = {}
        for kind, cmd in (("table", "idTable"), ("records", "idRecs")):
            out = os.path.join(tmp, f"{kind}.pkt")
            b.time(["./nexus", "-f", "packet", "-o", out, cmd, files[(big, True)]], reps=1)
            sizes[kind] = os.path.getsize(out)
            os.remove(out)
            b.record("TBL-2", f"{kind}.value_mib", sizes[kind] / 2**20, "MiB", f"{big} rows")

        slopes, totals = {}, {}
        for mode in ("table", "records"):
            series = "std." + mode
            ys = b.sweep(series, ns, lambda n: [std, mode, files[(n, True)], "headed"])
            c, s, r2 = fit_linear(ns, ys)
            slopes[series], totals[series] = s, ys[-1]
            b.record("TBL-1", f"{series}.b", s * 1e9, "ns", f"r2={r2:.4f} per row")
            b.record("TBL-1", f"{series}.c", c * 1e3, "ms")
        for series, cmd, headed, mode in CASES:
            ys = b.sweep(series, ns, lambda n: ["./nexus", cmd, files[(n, headed)]])
            c, s, r2 = fit_linear(ns, ys)
            ref = "std." + mode
            b.record("TBL-1", f"{series}.b", s * 1e9, "ns", f"r2={r2:.4f} per row")
            b.record("TBL-1", f"{series}.c", c * 1e3, "ms")
            b.record("TBL-1", f"{series}.ratio", s / slopes[ref], "x", f"per-row cost over {ref}")
            b.record("TBL-1", f"{series}.whole_ratio", ys[-1] / totals[ref], "x",
                     f"whole run at {big} rows over {ref}")

        def added(cmd_of, headed):
            base_sum, base_max = tree_peak(cmd_of(files[(0, headed)]), b.dir)
            top_sum, top_max = tree_peak(cmd_of(files[(big, headed)]), b.dir)
            return (top_sum - base_sum) * 1024, (top_max - base_max) * 1024

        for mode in ("table", "records"):
            s, m = added(lambda f: [std, mode, f, "headed"], True)
            b.record("TBL-2", f"std.{mode}.peak_ratio", s / sizes[mode], "x",
                     f"{s / 2**20:.1f} MiB added at {big} rows")
        for series, cmd, headed, mode in CASES:
            s, m = added(lambda f: ["./nexus", cmd, f], headed)
            b.record("TBL-2", f"{series}.peak_ratio", s / sizes[mode], "x",
                     f"{s / 2**20:.1f} MiB added, summed over processes, at {big} rows")
            b.record("TBL-2", f"{series}.peak_max_ratio", m / sizes[mode], "x",
                     f"{m / 2**20:.1f} MiB added in the largest process")
    finally:
        for f in os.listdir(tmp):
            os.remove(os.path.join(tmp, f))
        os.rmdir(tmp)
