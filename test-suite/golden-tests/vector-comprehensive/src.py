import numpy as np

# ---------------------------------------------------------------------------
# Real 10-element baseline: [1.0, 2.0, ..., 10.0]; sum = 55.0
# ---------------------------------------------------------------------------
def pyMakeReal10():
    return np.arange(1, 11, dtype=np.float64)

def pySumReal10(v):
    return float(np.sum(v))

# ---------------------------------------------------------------------------
# Element types. Distinct sums per width so a width-confusion bug surfaces.
# Each vector packs extremes of its width so any sign/zero-ext bug shows up.
# ---------------------------------------------------------------------------

# Int8: [-128, -1, 0, 127] -> -2
def pyMakeI8():
    return np.array([-128, -1, 0, 127], dtype=np.int8)
def pySumI8(v):
    return int(np.sum(v.astype(np.int64)))

# Int16: [-32768, -1, 0, 32767] -> -2
def pyMakeI16():
    return np.array([-32768, -1, 0, 32767], dtype=np.int16)
def pySumI16(v):
    return int(np.sum(v.astype(np.int64)))

# Int32: extremes
def pyMakeI32():
    return np.array([-2147483648, -1, 0, 2147483647], dtype=np.int32)
def pySumI32(v):
    return int(np.sum(v.astype(np.int64)))

# Int64: avoid INT64_MIN so we don't overflow morloc's Int (which is 64-bit).
# [-9223372036854775807, -1, 0, 9223372036854775806] -> -2
def pyMakeI64():
    return np.array([-9223372036854775807, -1, 0, 9223372036854775806],
                    dtype=np.int64)
def pySumI64(v):
    # numpy sum could overflow; use Python int via tolist.
    return int(sum(int(x) for x in v.tolist()))

# UInt8: [0, 1, 127, 255] -> 383
def pyMakeU8():
    return np.array([0, 1, 127, 255], dtype=np.uint8)
def pySumU8(v):
    return int(np.sum(v.astype(np.int64)))

# UInt16: [0, 1, 32767, 65535] -> 98303
def pyMakeU16():
    return np.array([0, 1, 32767, 65535], dtype=np.uint16)
def pySumU16(v):
    return int(np.sum(v.astype(np.int64)))

# UInt32: [0, 1, 65535, 4294967295] -> 4295032831
def pyMakeU32():
    return np.array([0, 1, 65535, 4294967295], dtype=np.uint32)
def pySumU32(v):
    return int(np.sum(v.astype(np.int64)))

# UInt64: [0, 1, 65535, 4294967295] -> 4295032831 (Int-safe under 64-bit Int)
def pyMakeU64():
    return np.array([0, 1, 65535, 4294967295], dtype=np.uint64)
def pySumU64(v):
    return int(sum(int(x) for x in v.tolist()))

# Float32: [0.5, 1.5, 2.5, 3.5] -> 8.0
def pyMakeF32():
    return np.array([0.5, 1.5, 2.5, 3.5], dtype=np.float32)
def pySumF32(v):
    return float(np.sum(v))

# Float64: [0.5, 1.5, 2.5, 3.5] -> 8.0
def pyMakeF64():
    return np.array([0.5, 1.5, 2.5, 3.5], dtype=np.float64)
def pySumF64(v):
    return float(np.sum(v))

# Width-aliasing probes: a single extreme value per width.
# I8 [-128]: must stay -128 (not 128 if accidentally read as uint8).
def pyMakeI8Min():
    return np.array([-128], dtype=np.int8)
def pySumI8Min(v):
    return int(np.sum(v.astype(np.int64)))

# U8 [255]: must stay 255 (not -1 if accidentally read as int8).
def pyMakeU8Max():
    return np.array([255], dtype=np.uint8)
def pySumU8Max(v):
    return int(np.sum(v.astype(np.int64)))

# ---------------------------------------------------------------------------
# Permutation probe: powers of two, return element at index 3 (== 8).
# A shuffle that preserved the multiset would change this value.
# ---------------------------------------------------------------------------
def pyMakePerm():
    return np.array([1, 2, 4, 8, 16, 32, 64, 128], dtype=np.int32)
def pyAtIdx3(v):
    return int(v[3])

# ---------------------------------------------------------------------------
# Big / threshold-straddle / edges
# ---------------------------------------------------------------------------

# 50000 doubles = 400 KB; values 0..49999; sum = 49999*50000/2 = 1249975000.
def pyMakeBig():
    return np.arange(50000, dtype=np.float64)
def pySumBig(v):
    return float(np.sum(v))

# Just under the 64 KB inline threshold: 8000 doubles = 64,000 bytes.
def pyMakeUnder():
    return np.zeros(8000, dtype=np.float64)

# Just over the 64 KB threshold: 8200 doubles = 65,600 bytes.
def pyMakeOver():
    return np.zeros(8200, dtype=np.float64)

def pyEmpty():
    return np.array([], dtype=np.float64)
def pySumEmpty(v):
    return float(np.sum(v))

def pySingle():
    return np.array([42.0], dtype=np.float64)
def pySumSingle(v):
    return float(np.sum(v))

# ---------------------------------------------------------------------------
# Zero-copy probes. Report numpy OWNDATA + size on a single line.
#
# Reasoning: in pymorloc.c, the only branch that produces OWNDATA=False
# is PyArray_SimpleNewFromData() over an SHM-resolved absptr (base_ptr ==
# NULL). The inline-packet branch always memcpys into a freshly allocated
# numpy buffer (OWNDATA=True). So OWNDATA=False is a sufficient witness
# that the SHM zero-copy path executed.
#
# Returning a string lets the golden output show whether the path flipped,
# rather than failing silently with a boolean.
# ---------------------------------------------------------------------------
def _report(v):
    own = bool(v.flags['OWNDATA']) if hasattr(v, 'flags') else None
    return "OWNDATA=%s,size=%d" % (str(own), int(v.size if hasattr(v, 'size') else len(v)))

def pyOwnDataLarge(v):
    return _report(v)

def pyOwnDataSmall(v):
    return _report(v)

def pyOwnDataAny(v):
    return _report(v)

# ---------------------------------------------------------------------------
# Polymorphic-in-size sum. Used by the literal-input tests where the
# vector length is determined by the input at runtime.
# ---------------------------------------------------------------------------
def pySumVecPoly(v):
    if hasattr(v, 'size') and v.size == 0:
        return 0.0
    if not hasattr(v, '__len__') or len(v) == 0:
        return 0.0
    return float(np.sum(v))

def pySumI32Poly(v):
    if hasattr(v, 'size') and v.size == 0:
        return 0
    if not hasattr(v, '__len__') or len(v) == 0:
        return 0
    return int(np.sum(v.astype(np.int64) if hasattr(v, 'astype') else v))
