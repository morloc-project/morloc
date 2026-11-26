"""Harness shared by the performance benchmarks.

A benchmark is a directory holding `bench.py`, which defines

    def run(b):  # b is a Bench
        ...

and the morloc sources it builds. `run.sh <name>` runs it. A benchmark
times commands, fits a model to the timings and records each fitted
parameter under the item ID it is evidence for. A run writes
`results/<run>/<name>.points.tsv` (every timed point) and
`results/<run>/<name>.fit.tsv` (fitted parameters); `compare.py` lines runs
up against each other and against the spec's `Goal:` and `Limit:` lines.
"""

import math
import os
import platform
import signal
import subprocess
import sys
import threading
import time

HERE = os.path.dirname(os.path.abspath(__file__))
RESULTS = os.path.join(HERE, "results")

UNITS = {"s": 1.0, "ms": 1e-3, "us": 1e-6, "ns": 1e-9}


def fmt_time(seconds):
    for unit in ("s", "ms", "us", "ns"):
        if abs(seconds) >= UNITS[unit] or unit == "ns":
            return f"{seconds / UNITS[unit]:.3g} {unit}"


class Timing:
    def __init__(self, walls, maxrss_kib, stdout, cpus=None, extra=None):
        self.walls = sorted(walls)
        self.maxrss_kib = maxrss_kib
        self.stdout = stdout
        # Per-run maxima from LAST (tree_pss_kib, dirty_kib), when sampled.
        self.extra = extra or {}
        # CPU seconds (user + system) of the process and its waited-for
        # descendants, per run; wall minus CPU is the time spent off-CPU.
        self.cpus = sorted(cpus or [])

    @property
    def cpu(self):
        return self.cpus[len(self.cpus) // 2] if self.cpus else None

    @property
    def best(self):
        return self.walls[0]

    @property
    def median(self):
        return self.walls[len(self.walls) // 2]


def timed(cmd, cwd, env=None, stdin=None, timeout=None):
    """Run cmd once; return (wall seconds, max RSS KiB of the largest
    descendant, stdout bytes). Fails loudly on a nonzero exit, and with
    `timeout` (seconds) kills cmd's session and raises TimeoutError."""
    return timed_cpu(cmd, cwd, env=env, stdin=stdin, timeout=timeout)[:3]


def meminfo_kib(*keys):
    """Sum of the named /proc/meminfo fields, in KiB."""
    total = 0
    try:
        for line in open("/proc/meminfo"):
            k, v = line.split(":", 1)
            if k in keys:
                total += int(v.split()[0])
    except OSError:
        pass
    return total


def session_pss_kib(sid):
    """PSS summed over every process in session `sid`, in KiB. Pools are
    not waited-for children of the nexus, but they stay in its session."""
    total = 0
    for d in os.listdir("/proc"):
        if not d.isdigit():
            continue
        try:
            with open(f"/proc/{d}/stat") as f:
                st = f.read()
            if int(st[st.rindex(")") + 2:].split()[3]) != sid:
                continue
            with open(f"/proc/{d}/smaps_rollup") as f:
                for line in f:
                    if line.startswith("Pss:"):
                        total += int(line.split()[1])
                        break
        except (OSError, ValueError, IndexError):
            continue
    return total


class TreeSampler:
    """Samples the PSS of a session every `period` seconds; `peak` is the
    largest sample. Short peaks between samples are missed."""

    def __init__(self, sid, period=0.05):
        self.sid, self.period, self.peak = sid, period, 0
        self.done = threading.Event()
        self.thread = threading.Thread(target=self.loop, daemon=True)
        self.thread.start()

    def loop(self):
        while not self.done.is_set():
            self.peak = max(self.peak, session_pss_kib(self.sid))
            self.done.wait(self.period)

    def stop(self):
        self.done.set()
        self.thread.join()
        return self.peak


# Set by a benchmark: measure the process tree's peak PSS in one extra,
# untimed run per point (sampling scans /proc and walks page tables, which
# would perturb the timed runs), and sync before every run.
OPTIONS = {"tree_mem": False, "sync": False}
LAST = {}


def take_last(acc):
    """Fold the last run's LAST samples into acc (maxima), then clear LAST."""
    for k, v in LAST.items():
        acc[k] = max(acc.get(k, 0), v)
    LAST.clear()
    return acc


def timed_cpu(cmd, cwd, env=None, stdin=None, sample=False, timeout=None):
    """timed, plus the CPU seconds of cmd and its waited-for descendants.
    With `sample`, LAST["tree_pss_kib"] is the run's sampled peak PSS over
    its whole session; with OPTIONS sync, the page cache is synced first
    and LAST["dirty_kib"] is Dirty+Writeback when the run starts."""
    full_env = dict(os.environ)
    if env:
        full_env.update(env)
    stdin_f = open(stdin, "rb") if stdin else subprocess.DEVNULL
    bufs = {}

    def drain(key, stream):
        bufs[key] = stream.read()

    if OPTIONS["sync"]:
        os.sync()
        LAST["dirty_kib"] = meminfo_kib("Dirty", "Writeback")
    t0 = time.perf_counter()
    p = subprocess.Popen(cmd, cwd=cwd, env=full_env, stdin=stdin_f,
                         stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                         start_new_session=sample or timeout is not None)
    sampler = TreeSampler(p.pid) if sample else None
    expired = threading.Event()

    def kill():
        expired.set()
        os.killpg(p.pid, signal.SIGKILL)

    timer = threading.Timer(timeout, kill) if timeout is not None else None
    if timer:
        timer.start()
    readers = [threading.Thread(target=drain, args=("out", p.stdout)),
               threading.Thread(target=drain, args=("err", p.stderr))]
    for r in readers:
        r.start()
    _, status, usage = os.wait4(p.pid, 0)
    t1 = time.perf_counter()
    if timer:
        timer.cancel()
    if sampler:
        LAST["tree_pss_kib"] = sampler.stop()
    for r in readers:
        r.join()
    p.returncode = os.waitstatus_to_exitcode(status)
    if stdin:
        stdin_f.close()
    if expired.is_set():
        raise TimeoutError(f"{cmd} in {cwd} did not finish in {timeout} s")
    if p.returncode != 0:
        sys.stderr.write(bufs["err"].decode(errors="replace"))
        raise RuntimeError(f"{cmd} exited {p.returncode}")
    return t1 - t0, usage.ru_maxrss, bufs["out"], usage.ru_utime + usage.ru_stime


def fit_linear(xs, ys):
    """Least squares y = c + b*x. Returns (c, b, r2)."""
    n = len(xs)
    mx = sum(xs) / n
    my = sum(ys) / n
    sxx = sum((x - mx) ** 2 for x in xs)
    sxy = sum((x - mx) * (y - my) for x, y in zip(xs, ys))
    b = sxy / sxx
    c = my - b * mx
    ss_tot = sum((y - my) ** 2 for y in ys)
    ss_res = sum((y - c - b * x) ** 2 for x, y in zip(xs, ys))
    r2 = 1.0 - ss_res / ss_tot if ss_tot > 0 else 1.0
    return c, b, r2


def fit_linear2(x1s, x2s, ys):
    """Least squares y = c + a1*x1 + a2*x2. Returns (c, a1, a2, r2)."""
    n = len(ys)
    rows = [[1.0, a, b] for a, b in zip(x1s, x2s)]
    m = [[sum(r[i] * r[j] for r in rows) for j in range(3)] +
         [sum(r[i] * y for r, y in zip(rows, ys))] for i in range(3)]
    for i in range(3):
        piv = max(range(i, 3), key=lambda r: abs(m[r][i]))
        m[i], m[piv] = m[piv], m[i]
        for r in range(3):
            if r != i:
                f = m[r][i] / m[i][i]
                m[r] = [a - f * bb for a, bb in zip(m[r], m[i])]
    c, a1, a2 = (m[i][3] / m[i][i] for i in range(3))
    my = sum(ys) / n
    ss_tot = sum((y - my) ** 2 for y in ys)
    ss_res = sum((y - c - a1 * u - a2 * v) ** 2 for u, v, y in zip(x1s, x2s, ys))
    return c, a1, a2, (1.0 - ss_res / ss_tot if ss_tot > 0 else 1.0)


def fit_power(xs, ys):
    """Least squares on logs: y = a * x^k. Returns (a, k, r2 of the log fit)."""
    lc, k, r2 = fit_linear([math.log(x) for x in xs], [math.log(y) for y in ys])
    return math.exp(lc), k, r2


def growth_exponent(xs, ys):
    """k in t(x) = c + a*x^k, for xs that double at each step. The
    differences between successive times grow as x^k whatever c is, so k is
    the power fitted to them. None with fewer than three points or a
    difference that is not positive. Returns (k, r2)."""
    if len(xs) < 3:
        return None
    ds = [b - a for a, b in zip(ys, ys[1:])]
    if any(d <= 0 for d in ds):
        return None
    _, k, r2 = fit_power(xs[:-1], ds)
    return k, r2


def fit_quadratic(xs, ys):
    """Least squares y = c + b*x + a*x^2. Returns (c, b, a, r2)."""
    s = [sum(x ** k for x in xs) for k in range(5)]
    t = [sum(y * x ** k for x, y in zip(xs, ys)) for k in range(3)]
    m = [[s[0], s[1], s[2], t[0]],
         [s[1], s[2], s[3], t[1]],
         [s[2], s[3], s[4], t[2]]]
    for i in range(3):
        piv = max(range(i, 3), key=lambda r: abs(m[r][i]))
        m[i], m[piv] = m[piv], m[i]
        for r in range(3):
            if r != i:
                f = m[r][i] / m[i][i]
                m[r] = [a - f * bb for a, bb in zip(m[r], m[i])]
    c, b, a = (m[i][3] / m[i][i] for i in range(3))
    my = sum(ys) / len(ys)
    ss_tot = sum((y - my) ** 2 for y in ys)
    ss_res = sum((y - c - b * x - a * x * x) ** 2 for x, y in zip(xs, ys))
    return c, b, a, (1.0 - ss_res / ss_tot if ss_tot > 0 else 1.0)


def morloc_version():
    try:
        return subprocess.run(["morloc", "--version"], capture_output=True,
                              text=True).stdout.strip()
    except OSError:
        return "?"


def compiler_commit():
    repo = os.path.abspath(os.path.join(HERE, "..", "..", ".."))
    try:
        rev = subprocess.run(["git", "-C", repo, "rev-parse", "--short", "HEAD"],
                             capture_output=True, text=True).stdout.strip()
        dirty = subprocess.run(["git", "-C", repo, "status", "--porcelain",
                                "--untracked-files=no"],
                               capture_output=True, text=True).stdout.strip()
        return rev + ("+dirty" if dirty else "")
    except OSError:
        return "?"


def cpu_name():
    try:
        for line in open("/proc/cpuinfo"):
            if line.startswith("model name"):
                return line.split(":", 1)[1].strip()
    except OSError:
        pass
    return platform.processor() or "?"


class Bench:
    def __init__(self, name, reps):
        self.name = name
        self.dir = os.path.join(HERE, name)
        self.reps = reps
        self.points = []
        self.fits = []
        self.started = time.strftime("%Y-%m-%d %H:%M")
        self.load = os.getloadavg()[0]
        self.load_max = self.load

    def path(self, *parts):
        return os.path.join(self.dir, *parts)

    def make(self, loc="main.loc", out="nexus", env=None):
        """Build a morloc program in the benchmark directory; returns build
        wall seconds."""
        wall, _, _ = timed(["morloc", "make", "-o", out, loc], self.dir, env=env)
        return wall

    def time(self, cmd, reps=None, env=None, stdin=None, cwd=None):
        """Time cmd `reps` times (default: the bench's). Returns a Timing."""
        walls, cpus = [], []
        rss = 0
        out = b""
        ext = {}
        for _ in range(reps or self.reps):
            w, r, out, c = timed_cpu(cmd, cwd or self.dir, env=env, stdin=stdin)
            walls.append(w)
            cpus.append(c)
            rss = max(rss, r)
            take_last(ext)
        if OPTIONS["tree_mem"]:
            timed_cpu(cmd, cwd or self.dir, env=env, stdin=stdin, sample=True)
            take_last(ext)
        return Timing(walls, rss, out, cpus, ext)

    def point(self, series, x, timing, **extra):
        load = os.getloadavg()[0]
        self.load_max = max(self.load_max, load)
        row = {"series": series, "x": x, "best_s": timing.best,
               "median_s": timing.median, "maxrss_kib": timing.maxrss_kib,
               "load": f"{load:.2f}"}
        if timing.cpu is not None:
            row["cpu_s"] = f"{timing.cpu:.4f}"
        row.update(timing.extra)
        row.update(extra)
        self.points.append(row)
        print(f"  {series:<24} x={x:<12} best {fmt_time(timing.best):>10}"
              f"  median {fmt_time(timing.median):>10}", flush=True)
        return timing.best

    def record(self, item, param, value, unit, note=""):
        """Record a fitted parameter as evidence for a spec item. `value`
        is in `unit`."""
        self.fits.append((item, param, value, unit, note))
        print(f"  => {item} {param} = {value:.4g} {unit} {note}", flush=True)

    def sweep(self, series, xs, cmd_of, reps=None, env=None, cwd_of=None, timeout=None):
        """Time cmd_of(x) for every x, interleaved: each repetition visits
        every x once, so a change in machine load during the sweep reaches
        all points rather than tilting the fit. Returns the best time per x."""
        walls = {x: [] for x in xs}
        cpus = {x: [] for x in xs}
        rss = {x: 0 for x in xs}
        ext = {x: {} for x in xs}
        for _ in range(reps or self.reps):
            for x in xs:
                w, r, _, c = timed_cpu(cmd_of(x), cwd_of(x) if cwd_of else self.dir, env=env,
                                       timeout=timeout)
                walls[x].append(w)
                cpus[x].append(c)
                rss[x] = max(rss[x], r)
                take_last(ext[x])
        if OPTIONS["tree_mem"]:
            for x in xs:
                timed_cpu(cmd_of(x), cwd_of(x) if cwd_of else self.dir, env=env, sample=True,
                          timeout=timeout)
                take_last(ext[x])
        return [self.point(series, x, Timing(walls[x], rss[x], b"", cpus[x], ext[x])) for x in xs]

    def sweep_variants(self, series, variants, xs, cmd_of, reps=None, cwd_of=None):
        """An in-run A/B: time cmd_of(x) under each variant's environment
        (`variants` maps a name to an env dict), visiting every variant and
        every x once per repetition, so load reaches all of them alike.
        Records series `<series>.<variant>`; returns {variant: [best per x]}."""
        walls = {(v, x): [] for v in variants for x in xs}
        cpus = {k: [] for k in walls}
        rss = {k: 0 for k in walls}
        ext = {k: {} for k in walls}
        for _ in range(reps or self.reps):
            for x in xs:
                for v, env in variants.items():
                    w, r, _, c = timed_cpu(cmd_of(x), cwd_of(x) if cwd_of else self.dir, env=env)
                    walls[v, x].append(w)
                    cpus[v, x].append(c)
                    rss[v, x] = max(rss[v, x], r)
                    take_last(ext[v, x])
        if OPTIONS["tree_mem"]:
            for x in xs:
                for v, env in variants.items():
                    timed_cpu(cmd_of(x), cwd_of(x) if cwd_of else self.dir, env=env, sample=True)
                    take_last(ext[v, x])
        return {v: [self.point(f"{series}.{v}", x,
                               Timing(walls[v, x], rss[v, x], b"", cpus[v, x], ext[v, x]))
                    for x in xs] for v in variants}

    def sweep_linear(self, item, series, xs, cmd_of, x_unit, b_unit="us",
                     c_unit="ms", reps=None, env=None, cwd_of=None):
        """Sweep, fit t = c + b*x and record c and b."""
        ys = self.sweep(series, xs, cmd_of, reps=reps, env=env, cwd_of=cwd_of)
        c, b, r2 = fit_linear(xs, ys)
        note = f"r2={r2:.4f} per {x_unit}"
        self.record(item, f"{series}.c", c / UNITS[c_unit], c_unit, f"r2={r2:.4f}")
        self.record(item, f"{series}.b", b / UNITS[b_unit], b_unit, note)
        return c, b, r2

    def check_point(self, series, x, cmd, cwd, limit_s):
        """Run cmd once, for --check. Returns [] if it succeeds within
        limit_s seconds, else a one-line failure."""
        try:
            wall, _, _ = timed(cmd, cwd, timeout=limit_s)
        except (TimeoutError, RuntimeError) as e:
            print(f"  {series:<24} x={x:<12} FAIL", flush=True)
            return [f"{self.name} {series} x={x}: {e}"]
        print(f"  {series:<24} x={x:<12} {fmt_time(wall):>10}  ok (limit {limit_s} s)", flush=True)
        return []

    def save(self, run_dir):
        os.makedirs(run_dir, exist_ok=True)
        head = (f"# {self.name} {self.started} load={self.load:.2f} "
                f"load_max={self.load_max:.2f} "
                f"morloc={morloc_version()} commit={compiler_commit()} "
                f"cpu={cpu_name()} cores={os.cpu_count()}\n")
        keys = []
        for p in self.points:
            for k in p:
                if k not in keys:
                    keys.append(k)
        with open(os.path.join(run_dir, self.name + ".points.tsv"), "w") as f:
            f.write(head)
            f.write("\t".join(keys) + "\n")
            for p in self.points:
                f.write("\t".join(str(p.get(k, "")) for k in keys) + "\n")
        with open(os.path.join(run_dir, self.name + ".fit.tsv"), "w") as f:
            f.write(head)
            f.write("item\tparam\tvalue\tunit\tnote\n")
            for item, param, value, unit, note in self.fits:
                f.write(f"{item}\t{param}\t{value:.6g}\t{unit}\t{note}\n")


def default_run_name(label=None):
    name = time.strftime("%Y-%m-%d-%H%M") + "-" + compiler_commit()
    return name + "-" + label if label else name


def main():
    sys.path.insert(0, HERE)
    # A bench imports `perf`; make that this module, so the OPTIONS it sets
    # are the ones timed_cpu reads.
    sys.modules.setdefault("perf", sys.modules[__name__])
    import argparse
    import importlib.util
    ap = argparse.ArgumentParser()
    ap.add_argument("name")
    ap.add_argument("--reps", type=int, default=7)
    ap.add_argument("--quick", action="store_true",
                    help="smaller sweeps; results are not saved")
    ap.add_argument("--check", action="store_true",
                    help="the bench's check(b): one run per series under a time "
                         "limit; exit 1 on any failure; nothing saved")
    ap.add_argument("--run", help="results/<RUN> to write into (default: date-commit)")
    ap.add_argument("--label", help="appended to the default run name")
    a = ap.parse_args()
    path = os.path.join(HERE, a.name, "bench.py")
    spec = importlib.util.spec_from_file_location("bench_" + a.name.replace("-", "_"), path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    b = Bench(a.name, 2 if a.quick else a.reps)
    b.quick = a.quick
    if a.check:
        if not hasattr(mod, "check"):
            sys.exit(f"{a.name} has no check mode")
        print(f"{a.name}: check, load {b.load:.2f}", flush=True)
        failures = mod.check(b)
        for f in failures:
            print(f"FAIL {f}", file=sys.stderr)
        sys.exit(1 if failures else 0)
    print(f"{a.name}: load {b.load:.2f}, {b.reps} reps", flush=True)
    mod.run(b)
    if not a.quick:
        run_dir = os.path.join(RESULTS, a.run or default_run_name(a.label))
        b.save(run_dir)
        print(f"saved to {run_dir}", flush=True)


if __name__ == "__main__":
    main()
