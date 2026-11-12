"""R @write of atomic vectors to a stream file, with a C++ control
(ENC-5). Each kind is timed writing n elements and generating them alone;
the difference of the slopes is the cost of encoding and writing one
element."""

import os
import shutil
import tempfile

# kind: bytes per element on the wire
KINDS = {"Reals": 8, "Ints": 4, "Bools": 1}
SIZES = [0, 4000000, 8000000, 16000000]


def run(b):
    b.make(loc="r.loc", out="nexus-r")
    b.make(loc="cpp.loc", out="nexus-cpp")
    ns = [0, 1000000] if b.quick else SIZES
    tmp = tempfile.mkdtemp(prefix="perf-swr-")
    try:
        for lang in ("r", "cpp"):
            exe = f"./nexus-{lang}"
            for kind, width in KINDS.items():
                out = os.path.join(tmp, f"{lang}-{kind}.mlc")
                _, bw, _ = b.sweep_linear("ENC-5", f"{lang}.{kind}.write", ns,
                                          lambda n: [exe, "write" + kind, str(n), out],
                                          "element", b_unit="ns")
                _, bg, _ = b.sweep_linear("ENC-5", f"{lang}.{kind}.gen", ns,
                                          lambda n: [exe, "count" + kind, str(n)],
                                          "element", b_unit="ns")
                d = bw - bg
                rate = f"{width / d / 1e6:.0f} MB/s" if d > 0 else "unresolved"
                b.record("ENC-5", f"{lang}.{kind}.encode", d * 1e9, "ns",
                         f"write slope over generation slope; {rate} at {width} B per element")
    finally:
        shutil.rmtree(tmp)
