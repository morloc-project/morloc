"""@save and @load of a [Real] from a C++ pool, and a [Real] written to
@stdout and rendered as JSON (ENC-6, ENC-7). Each is timed beside
generating the list alone; the difference of the slopes is the cost of
the operation per element."""

import os
import shutil
import tempfile

SIZES = [0, 4000000, 8000000, 16000000]
LEVELS = [0, 3]


def run(b):
    b.make()
    ns = [0, 1000000] if b.quick else SIZES
    tmp = tempfile.mkdtemp(prefix="perf-save-", dir="/dev/shm" if os.path.isdir("/dev/shm") else None)
    try:
        _, bg, _ = b.sweep_linear("ENC-6", "gen", ns,
                                  lambda n: ["./nexus", "countReals", str(n)],
                                  "element", b_unit="ns")
        for lvl in LEVELS:
            files = {n: os.path.join(tmp, f"s{lvl}-{n}.mlc") for n in ns}
            _, bs, _ = b.sweep_linear("ENC-6", f"save{lvl}", ns,
                                      lambda n: ["./nexus", "saveReals", str(lvl), str(n), files[n]],
                                      "element", b_unit="ns")
            d = bs - bg
            rate = f"{8 / d / 1e6:.0f} MB/s" if d > 0 else "unresolved"
            b.record("ENC-6", f"save{lvl}.encode", d * 1e9, "ns",
                     f"save slope over generation slope; {rate} at 8 B per element")
            b.sweep_linear("ENC-6", f"load{lvl}", ns,
                           lambda n: ["./nexus", "loadReals", files[n]],
                           "element", b_unit="ns")
        _, bj, _ = b.sweep_linear("ENC-7", "stdout.json", ns,
                                  lambda n: ["./nexus", "streamReals", str(n)],
                                  "element", b_unit="ns")
        b.record("ENC-7", "stdout.json.render", (bj - bg) * 1e9, "ns",
                 "stream slope over generation slope")
    finally:
        shutil.rmtree(tmp)
