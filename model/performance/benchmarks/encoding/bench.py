import os
import tempfile

KINDS = ["Ints", "U64s", "Reals", "Bools", "Strs"]
FORMS = ["json", "mpk", "voidstar", "packet"]
SIZES = [0, 1000000, 2000000, 4000000]


def run(b):
    b.make()
    tmp = tempfile.mkdtemp(prefix="perf-enc-")
    try:
        # ENC-2: exact bytes per element of each form, from the output of
        # two lengths.
        for kind in KINDS:
            for form in FORMS:
                sizes = []
                for n in (1000, 2000):
                    p = os.path.join(tmp, f"w.{form}")
                    b.time(["./nexus", "-f", form, "-o", p, "mk" + kind, str(n)], reps=1)
                    sizes.append(os.path.getsize(p))
                    os.remove(p)
                b.record("ENC-2", f"{kind}.{form}.bytes", (sizes[1] - sizes[0]) / 1000,
                         "B", "per element")
        ns = SIZES[:2] if b.quick else SIZES
        for kind in KINDS[1:]:
            for form in FORMS:
                # ENC-3: writing the result in a form.
                out = os.path.join(tmp, f"o.{form}")
                b.sweep_linear("ENC-3", f"out:{kind}.{form}", ns,
                               lambda n: ["./nexus", "-f", form, "-o", out, "mk" + kind, str(n)],
                               "element", b_unit="ns")
                os.remove(out)
                # ENC-1: reading an argument from a file in a form. A
                # voidstar file of a large value cannot be read back
                # (runtime/issues/nexus-cli.md).
                if form == "voidstar":
                    continue
                files = {}
                for n in ns:
                    files[n] = os.path.join(tmp, f"in{n}.{form}")
                    b.time(["./nexus", "-f", form, "-o", files[n], "mk" + kind, str(n)], reps=1)
                b.sweep_linear("ENC-1", f"in:{kind}.{form}", ns,
                               lambda n: ["./nexus", "len" + kind, files[n]],
                               "element", b_unit="ns")
                for p in files.values():
                    os.remove(p)
    finally:
        for f in os.listdir(tmp):
            os.remove(os.path.join(tmp, f))
        os.rmdir(tmp)
