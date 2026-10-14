#!/usr/bin/env python3
"""Line up benchmark runs against each other and against the spec.

    python3 compare.py                      # the latest run against the one before
    python3 compare.py RUN [RUN ...]        # these runs, oldest first, newest last
    python3 compare.py --last 4             # the four latest runs
    python3 compare.py --bench xl-calls --item XL-1 RUN_A RUN_B

A RUN is a directory under `results/` or a path to one. The table has one
row per recorded parameter: its value in each run, the change of the newest
run against the one before it, the spec's goal and limit, and a status:

    FAIL      the newest value breaks the item's `Limit:`
    REGRESS   the newest value is worse than the previous run's by more
              than the tolerance. Lower is better unless the item's Limit
              or Goal says `>=`, or the parameter is a speedup.
    better    better than the previous run by more than the tolerance
    goal      meets the item's `Goal:`

Exits 1 on any FAIL or REGRESS.
"""

import argparse
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
SPEC = os.path.dirname(HERE)
RESULTS = os.path.join(HERE, "results")

COND = re.compile(r"(\S+) (<=|>=) (-?[0-9.]+(?:e-?\d+)?) ?(\S*)$")


def spec_conditions():
    """{(item, param): {"goal": (op, value, unit), "limit": (...)}} and a
    list of malformed lines."""
    conds, errors = {}, []
    for fn in sorted(os.listdir(SPEC)):
        if not fn.endswith(".md") or fn == "README.md":
            continue
        item = None
        for n, line in enumerate(open(os.path.join(SPEC, fn)), 1):
            m = re.match(r"### ([A-Z]+-\d+) ", line)
            if m:
                item = m[1]
                continue
            for field in ("Goal", "Limit"):
                if item and line.startswith(field + ":"):
                    for text in line.split(":", 1)[1].split(","):
                        m = COND.match(text.strip())
                        if not m:
                            errors.append(f"{fn}:{n}: malformed {field} '{text.strip()}'")
                            continue
                        conds.setdefault((item, m[1]), {})[field.lower()] = \
                            (m[2], float(m[3]), m[4])
    return conds, errors


def load_run(path):
    """{bench: (header, {(item, param): (value, unit)})}"""
    out = {}
    for fn in sorted(os.listdir(path)):
        if not fn.endswith(".fit.tsv"):
            continue
        bench = fn[:-len(".fit.tsv")]
        head, rows = "", {}
        for line in open(os.path.join(path, fn)):
            if line.startswith("#"):
                head = line[1:].strip()
            elif not line.startswith("item\t"):
                item, param, value, unit, _ = line.rstrip("\n").split("\t")
                rows[item, param] = (float(value), unit)
        out[bench] = (head, rows)
    return out


def resolve(run):
    path = run if os.path.isdir(run) else os.path.join(RESULTS, run)
    if not os.path.isdir(path):
        sys.exit(f"no run {run}")
    return path


def runs_available():
    if not os.path.isdir(RESULTS):
        return []
    return sorted(d for d in os.listdir(RESULTS) if os.path.isdir(os.path.join(RESULTS, d)))


def direction_of(param, limit, goal):
    """`<=` when lower is better. The item's Limit or Goal decides; without
    one, lower is better unless the parameter is a speedup."""
    if limit or goal:
        return (limit or goal)[0]
    return ">=" if "speedup" in param else "<="


def holds(op, value, limit):
    return value <= limit if op == "<=" else value >= limit


def fmt(v):
    return "-" if v is None else f"{v:.4g}"


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("runs", nargs="*")
    ap.add_argument("--last", type=int, help="compare the N latest runs")
    ap.add_argument("--bench", help="only this benchmark")
    ap.add_argument("--item", help="only this item (XL-1)")
    ap.add_argument("--tolerance", type=float, default=0.10,
                    help="relative change counted as a regression (default 0.10)")
    ap.add_argument("--changed", action="store_true",
                    help="only rows with a status")
    a = ap.parse_args()

    names = a.runs or runs_available()[-(a.last or 2):]
    if not names:
        sys.exit("no runs in results/")
    paths = [resolve(r) for r in names]
    runs = [load_run(p) for p in paths]
    conds, errors = spec_conditions()
    for e in errors:
        print(e)

    print("runs, oldest first:")
    for i, (name, run) in enumerate(zip(names, runs)):
        heads = {h for h, _ in run.values()}
        print(f"  [{i}] {os.path.basename(name.rstrip('/'))}")
        for h in sorted(heads):
            print(f"      {h}")

    benches = sorted({bn for run in runs for bn in run})
    if a.bench:
        benches = [bn for bn in benches if bn == a.bench]
    header = ["item", "param", "unit"] + [f"[{i}]" for i in range(len(runs))] + \
             ["change", "goal", "limit", "status"]
    rows = []
    fails = regressions = 0
    for bench in benches:
        keys = []
        for run in runs:
            for k in run.get(bench, ("", {}))[1]:
                if k not in keys:
                    keys.append(k)
        for item, param in keys:
            if a.item and item != a.item:
                continue
            vals = [run.get(bench, ("", {}))[1].get((item, param)) for run in runs]
            unit = next((v[1] for v in vals if v), "")
            nums = [v[0] if v else None for v in vals]
            c = conds.get((item, param), {})
            goal, limit = c.get("goal"), c.get("limit")
            direction = direction_of(param, limit, goal)
            new = nums[-1]
            prev = next((x for x in reversed(nums[:-1]) if x is not None), None)
            change, status = "", []
            if new is not None and prev not in (None, 0):
                rel = (new - prev) / abs(prev)
                change = f"{rel * 100:+.0f}%"
                if direction:
                    worse = rel > a.tolerance if direction == "<=" else rel < -a.tolerance
                    better = rel < -a.tolerance if direction == "<=" else rel > a.tolerance
                    if worse:
                        status.append("REGRESS")
                    elif better:
                        status.append("better")
            if new is not None and goal and holds(goal[0], new, goal[1]):
                status.append("goal")
            if new is not None and limit and not holds(limit[0], new, limit[1]):
                status.insert(0, "FAIL")
            fails += "FAIL" in status
            regressions += "REGRESS" in status
            status = " ".join(status)
            if a.changed and not status:
                continue
            show = lambda t: f"{t[0]} {t[1]:g}" if t else ""
            rows.append([item, param, unit] + [fmt(x) for x in nums] +
                        [change, show(goal), show(limit), status])

    widths = [max(len(str(r[i])) for r in [header] + rows) for i in range(len(header))]
    for r in [header] + rows:
        print("  ".join(str(x).ljust(w) for x, w in zip(r, widths)).rstrip())
    print(f"{len(rows)} rows, {fails} FAIL, {regressions} REGRESS "
          f"(tolerance {a.tolerance:.0%})")
    sys.exit(1 if fails or regressions or errors else 0)


if __name__ == "__main__":
    main()
