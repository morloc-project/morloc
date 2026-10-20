import json
import os
import tempfile

SIZES = [0, 250000, 500000, 1000000]


def values(kind, n):
    if kind == "Strs":
        return [f"s{i}" for i in range(n)]
    if kind == "OptReals":
        return [None if i % 2 else i * 0.5 for i in range(n)]
    return [[i] for i in range(n)]


def run(b):
    b.make()
    tmp = tempfile.mkdtemp(prefix="perf-pure-")
    ns = SIZES[:2] if b.quick else SIZES
    try:
        for kind in ("Strs", "OptReals", "Nested"):
            files = {}
            for n in ns:
                files[n] = os.path.join(tmp, f"{kind}{n}.json")
                with open(files[n], "w") as f:
                    json.dump(values(kind, n), f)
            out = os.path.join(tmp, "out.json")
            # ENC-4: a pure command reads a JSON argument file and writes
            # the value back.
            b.sweep_linear("ENC-4", f"pure:{kind}.json", ns,
                           lambda n: ["./nexus", "-o", out, "id" + kind, files[n]],
                           "element", b_unit="ns")
    finally:
        for f in os.listdir(tmp):
            os.remove(os.path.join(tmp, f))
        os.rmdir(tmp)
