import struct
import pymorloc


def probe(size, data):
    p = bytearray(pymorloc.put_value([1, 2, 3], "au1"))
    if p[13] != 0:
        return "not inline"
    base = 32 + struct.unpack_from("<I", p, 20)[0]
    if size >= 0:
        struct.pack_into("<Q", p, base, size)
    if data >= 0:
        struct.pack_into("<q", p, base + 8, data)
    try:
        return "ok %d" % len(pymorloc.get_value(bytes(p), "au1"))
    except Exception:
        return "refused"
