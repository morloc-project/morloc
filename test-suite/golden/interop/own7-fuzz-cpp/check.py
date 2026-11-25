"""Compare a run of every case with the oracle's (built with
MORLOC_NO_MOVES=1): a case is printed with its value and copies, and is
BAD when it failed, its value differs from the oracle's, or it copied more
than the oracle did. A third run, of a sanitizer build, must agree with
the first exactly."""

import json
import sys


def load(path):
    out = {}
    for line in open(path):
        name, _, rest = line.strip().partition(" ")
        try:
            out[name] = json.loads(rest)
        except ValueError:
            out[name] = None
    return out


run, oracle = load(sys.argv[1]), load(sys.argv[2])
checked = load(sys.argv[3]) if len(sys.argv) > 3 else None
bad = 0
for name, got in run.items():
    want = oracle.get(name)
    if got is None or want is None:
        status = "BAD failed"
    elif got[0] != want[0]:
        status = "BAD value, oracle %s" % want[0]
    elif got[1] > want[1]:
        status = "BAD copies, oracle %s" % want[1]
    elif checked is not None and checked.get(name) != got:
        status = "BAD sanitizer run %s" % checked.get(name)
    else:
        status = "ok"
    if status != "ok":
        bad += 1
    shown = "failed" if got is None else "%s %s" % (got[0], got[1])
    print(name, shown, status)
print("bad", bad)
