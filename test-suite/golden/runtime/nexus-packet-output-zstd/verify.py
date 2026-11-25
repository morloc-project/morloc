"""Probe the on-disk packets produced by the Makefile and emit
a deterministic, line-oriented summary that diffs against exp.txt.

For each scenario:
  - reads the 32-byte packet header (magic, source, format, compression,
    offset = metadata length, length = payload length)
  - asserts source = MESG and compression matches the requested -z mode
  - asserts format = VOIDSTAR for SHM path, MSGPACK for the FILE path
  - asserts the big-data payload is more than 8 bytes (i.e. the writer
    did NOT leave a bare RPTR pointer on disk)
  - asserts the -z 3 file is smaller than the -z 0 file for the big
    scenarios (zstd actually fired and compressed something redundant)
"""

import os
import sys

PACKET_MAGIC = 0x0707F86D
SOURCE_MESG = 0x00
FORMAT_MSGPACK = 0x01
FORMAT_VOIDSTAR = 0x04
COMP_NONE = 0x00
COMP_ZSTD = 0x01

SCENARIOS = [
    # (label, file, expected source, expected format, expected compression)
    ("small-z0",     "pkt-small-z0.dat",     SOURCE_MESG, FORMAT_VOIDSTAR, COMP_NONE),
    ("small-z3",     "pkt-small-z3.dat",     SOURCE_MESG, FORMAT_VOIDSTAR, COMP_ZSTD),
    ("big-shm-z0",   "pkt-big-shm-z0.dat",   SOURCE_MESG, FORMAT_VOIDSTAR, COMP_NONE),
    ("big-shm-z3",   "pkt-big-shm-z3.dat",   SOURCE_MESG, FORMAT_VOIDSTAR, COMP_ZSTD),
    ("big-noshm-z0", "pkt-big-noshm-z0.dat", SOURCE_MESG, FORMAT_MSGPACK,  COMP_NONE),
    ("big-noshm-z3", "pkt-big-noshm-z3.dat", SOURCE_MESG, FORMAT_MSGPACK,  COMP_ZSTD),
]


def parse_header(path):
    with open(path, "rb") as f:
        h = f.read(32)
    return {
        "magic":       int.from_bytes(h[0:4], "little"),
        "source":      h[13],
        "format":      h[14],
        "compression": h[15],
        "offset":      int.from_bytes(h[20:24], "little"),
        "length":      int.from_bytes(h[24:32], "little"),
    }


def verdict(observed, expected):
    return "PASS" if observed == expected else f"FAIL (got 0x{observed:02x}, want 0x{expected:02x})"


def main():
    print("=== header bytes: source / format / compression ===")
    for label, path, esrc, efmt, ecmp in SCENARIOS:
        h = parse_header(path)
        magic_ok = "ok" if h["magic"] == PACKET_MAGIC else "bad-magic"
        print(
            f"{label+':':<16} "
            f"source={verdict(h['source'], esrc):<4} "
            f"format={verdict(h['format'], efmt):<4} "
            f"compression={verdict(h['compression'], ecmp):<4} "
            f"magic={magic_ok}"
        )

    print("=== big -z 0 payload is inlined, not an 8-byte pointer ===")
    for label in ("big-shm-z0", "big-noshm-z0"):
        path = next(p for l, p, _, _, _ in SCENARIOS if l == label)
        plen = parse_header(path)["length"]
        ok = plen > 8
        print(f"{label}: {'PASS' if ok else f'FAIL (payload len {plen} <= 8)'}")

    print("=== big -z 3 file shrinks vs -z 0 (zstd actually fired) ===")
    for label, z0, z3 in [
        ("big-shm",   "pkt-big-shm-z0.dat",   "pkt-big-shm-z3.dat"),
        ("big-noshm", "pkt-big-noshm-z0.dat", "pkt-big-noshm-z3.dat"),
    ]:
        s0 = os.path.getsize(z0)
        s3 = os.path.getsize(z3)
        ok = s3 < s0
        print(f"{label}: {'PASS' if ok else f'FAIL ({s0} -> {s3} bytes)'}")


if __name__ == "__main__":
    main()
