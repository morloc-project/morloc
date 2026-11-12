"""64 Str bodies of n bytes written to @stdout from a C++ pool, as raw
render output and as a `-f packet` stream, into /dev/null (ENC-8). Each
is timed beside building the bodies alone; the difference of the slopes
is the cost of moving one byte to stdout. A C++ loop writing the same
bodies with write(2) is the control."""

import os
import shutil
import subprocess
import tempfile

K = 64
SIZES = [0, 4194304, 8388608, 16777216]

CONTROL = r"""
#include <string>
#include <unistd.h>
#include <cstdlib>
int main(int argc, char** argv) {
  long n = atol(argv[1]);
  for (int k = 0; k < 64; k++) {
    std::string s((size_t)n, 'a');
    const char* p = s.data();
    size_t left = s.size();
    while (left) {
      ssize_t r = write(1, p, left);
      if (r <= 0) return 1;
      p += r;
      left -= r;
    }
  }
  return 0;
}
"""


def to_null(cmd):
    return ["sh", "-c", 'exec "$@" > /dev/null', "sh"] + cmd


def run(b):
    b.make()
    ns = [0, 1048576] if b.quick else SIZES
    tmp = tempfile.mkdtemp(prefix="perf-stdout-")
    try:
        ctl = os.path.join(tmp, "ctl")
        src = ctl + ".cpp"
        with open(src, "w") as f:
            f.write(CONTROL)
        subprocess.run(["g++", "-O2", "-o", ctl, src], check=True)
        _, bg, _ = b.sweep_linear("ENC-8", "gen", ns,
                                  lambda n: ["./nexus", "genOnly", str(K), str(n)],
                                  "byte per body", b_unit="ns")
        _, bc, _ = b.sweep_linear("ENC-8", "control", ns,
                                  lambda n: to_null([ctl, str(n)]),
                                  "byte per body", b_unit="ns")
        for form, cmd in (("raw", ["./nexus", "rawOut"]),
                          ("packet", ["./nexus", "-f", "packet", "packetOut"])):
            _, bw, _ = b.sweep_linear("ENC-8", form, ns,
                                      lambda n: to_null(cmd + [str(n)]),
                                      "byte per body", b_unit="ns")
            d = (bw - bg) / K
            rate = f"{1 / d / 1e9:.1f} GB/s" if d > 0 else "unresolved"
            b.record("ENC-8", f"{form}.write", d * 1e9, "ns",
                     f"{form} slope over generation slope, per byte; {rate}")
        b.record("ENC-8", "control.write", bc / K * 1e9, "ns",
                 "write(2) of the bodies, building included, per byte")
    finally:
        shutil.rmtree(tmp)
