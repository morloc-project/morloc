# Write prefixes of a stream file: through its last sub-packet (no footer),
# into its footer, and into its first sub-packet's header.
import sys

b = open(sys.argv[1], "rb").read()
open("nofooter.pkt", "wb").write(b[: len(b) - 8 - int.from_bytes(b[-8:-4], "little")])
open("infooter.pkt", "wb").write(b[: len(b) - 20])
first_sub = b.index(b"\x6d\xf8\x07\x07", 32)
open("inheader.pkt", "wb").write(b[: first_sub + 10])
