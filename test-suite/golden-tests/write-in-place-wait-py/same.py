# Compare two stream files byte for byte, apart from the writer's pid and
# times in the footer's diagnostics block.
import struct
import sys

def masked(path):
    b = bytearray(open(path, "rb").read())
    hdr = b"mmh\x06" + struct.pack("<I", 160)
    at = b.rfind(hdr)
    if at < 0:
        sys.exit(f"{path}: no diagnostics block")
    body = at + 8
    for off, n in [(4, 4), (24, 8), (80, 16)]:
        b[body + off:body + off + n] = bytes(n)
    return b

a, b = masked(sys.argv[1]), masked(sys.argv[2])
if a == b:
    print("same")
else:
    at = next((i for i, (x, y) in enumerate(zip(a, b)) if x != y), min(len(a), len(b)))
    print(f"differ at byte {at} ({len(a)} vs {len(b)} bytes)")
