# Write a call packet of `count` (manifold mid) around the argument packet.
import struct, sys
mid, arg, out = int(sys.argv[1]), open(sys.argv[2], "rb").read(), sys.argv[3]
head = arg[:12] + bytes([1, 0, 0, 0]) + struct.pack("<IIQ", mid, 0, len(arg))
open(out, "wb").write(head + arg)
