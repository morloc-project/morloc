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
    def __init__(self, walls, maxrss_kib, stdout):
        self.walls = sorted(walls)
        self.maxrss_kib = maxrss_kib
        self.stdout = stdout

    @property
    def best(self):
        return self.walls[0]

    @property
    def median(self):
        return self.walls[len(self.walls) // 2]


def timed(cmd, cwd, env=None, stdin=None):
    """Run cmd once; return (wall seconds, max RSS KiB of the largest
    descendant, stdout bytes). Fails loudly on a nonzero exit."""
    full_env = dict(os.environ)
    if env:
        full_env.update(env)
    stdin_f = open(stdin, "rb") if stdin else subprocess.DEVNULL
    bufs = {}

    def drain(key, stream):
        bufs[key] = stream.read()

    t0 = time.perf_counter()
    p = subprocess.Popen(cmd, cwd=cwd, env=full_env, stdin=stdin_f,
                         stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    readers = [threading.Thread(target=drain, args=("out", p.stdout)),
               threading.Thread(target=drain, args=("err", p.stderr))]
    for r in readers:
        r.start()
    _, status, usage = os.wait4(p.pid, 0)
    t1 = time.perf_counter()
    for r in readers:
        r.join()
    p.returncode = os.waitstatus_to_exitcode(status)
    if stdin:
        stdin_f.close()
    if p.returncode != 0:
        sys.stderr.write(bufs["err"].decode(errors="replace"))
        raise RuntimeError(f"{cmd} exited {p.returncode}")
    return t1 - t0, usage.ru_maxrss, bufs["out"]


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

    def path(self, *parts):
        return os.path.join(self.dir, *parts)

    def make(self, loc="main.loc", out="nexus", env=None):
        """Build a morloc program in the benchmark directory; returns build
        wall seconds."""
        wall, _, _ = timed(["morloc", "make", "-o", out, loc], self.dir, env=env)
        return wall

    def time(self, cmd, reps=None, env=None, stdin=None, cwd=None):
        """Time cmd `reps` times (default: the bench's). Returns a Timing."""
        walls = []
        rss = 0
        out = b""
        for _ in range(reps or self.reps):
            w, r, out = timed(cmd, cwd or self.dir, env=env, stdin=stdin)
            walls.append(w)
            rss = max(rss, r)
        return Timing(walls, rss, out)

    def point(self, series, x, timing, **extra):
        row = {"series": series, "x": x, "best_s": timing.best,
               "median_s": timing.median, "maxrss_kib": timing.maxrss_kib}
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

    def sweep(self, series, xs, cmd_of, reps=None, env=None, cwd_of=None):
        """Time cmd_of(x) for every x, interleaved: each repetition visits
        every x once, so a change in machine load during the sweep reaches
        all points rather than tilting the fit. Returns the best time per x."""
        walls = {x: [] for x in xs}
        rss = {x: 0 for x in xs}
        for _ in range(reps or self.reps):
            for x in xs:
                w, r, _ = timed(cmd_of(x), cwd_of(x) if cwd_of else self.dir, env=env)
                walls[x].append(w)
                rss[x] = max(rss[x], r)
        return [self.point(series, x, Timing(walls[x], rss[x], b"")) for x in xs]

    def sweep_linear(self, item, series, xs, cmd_of, x_unit, b_unit="us",
                     c_unit="ms", reps=None, env=None, cwd_of=None):
        """Sweep, fit t = c + b*x and record c and b."""
        ys = self.sweep(series, xs, cmd_of, reps=reps, env=env, cwd_of=cwd_of)
        c, b, r2 = fit_linear(xs, ys)
        note = f"r2={r2:.4f} per {x_unit}"
        self.record(item, f"{series}.c", c / UNITS[c_unit], c_unit, f"r2={r2:.4f}")
        self.record(item, f"{series}.b", b / UNITS[b_unit], b_unit, note)
        return c, b, r2

    def save(self, run_dir):
        os.makedirs(run_dir, exist_ok=True)
        head = (f"# {self.name} {self.started} load={self.load:.2f} "
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
    import argparse
    import importlib.util
    ap = argparse.ArgumentParser()
    ap.add_argument("name")
    ap.add_argument("--reps", type=int, default=5)
    ap.add_argument("--quick", action="store_true",
                    help="smaller sweeps; results are not saved")
    ap.add_argument("--run", help="results/<RUN> to write into (default: date-commit)")
    ap.add_argument("--label", help="appended to the default run name")
    a = ap.parse_args()
    path = os.path.join(HERE, a.name, "bench.py")
    spec = importlib.util.spec_from_file_location("bench_" + a.name.replace("-", "_"), path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    b = Bench(a.name, 2 if a.quick else a.reps)
    b.quick = a.quick
    print(f"{a.name}: load {b.load:.2f}, {b.reps} reps", flush=True)
    mod.run(b)
    if not a.quick:
        run_dir = os.path.join(RESULTS, a.run or default_run_name(a.label))
        b.save(run_dir)
        print(f"saved to {run_dir}", flush=True)


if __name__ == "__main__":
    main()
