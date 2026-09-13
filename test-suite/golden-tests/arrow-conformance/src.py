"""Canonical Arrow table fixture and describer, Python side.

Every language in the matrix builds the same table from the same literal
values and prints the same description, so exp.txt is one text repeated
once per (writer, reader) pair. The description is pure ASCII: string and
binary payloads print each byte outside 0x20..0x7e (and backslash) as
\\xNN, floats print with %g, temporal columns print their integer storage.
"""

import pyarrow as pa

I64_MIN = -9223372036854775808
I64_MAX = 9223372036854775807
U64_MAX = 18446744073709551615

# Column name, arrow type, five values with a null in slot 1.
COLUMNS = [
    ("b",   pa.bool_(),                    [True, None, False, True, False]),
    ("i8",  pa.int8(),                     [-128, None, 0, 127, 1]),
    ("i16", pa.int16(),                    [-32768, None, 0, 32767, 2]),
    ("i32", pa.int32(),                    [-2147483648, None, 0, 2147483647, 3]),
    ("i64", pa.int64(),                    [I64_MIN, None, 0, I64_MAX, 4]),
    ("u8",  pa.uint8(),                    [0, None, 255, 7, 5]),
    ("u16", pa.uint16(),                   [0, None, 65535, 7, 6]),
    ("u32", pa.uint32(),                   [0, None, 4294967295, 7, 7]),
    ("u64", pa.uint64(),                   [0, None, U64_MAX, 7, 8]),
    ("f32", pa.float32(),                  [0.5, None, -1.5, 100.125, 3.0]),
    ("f64", pa.float64(),                  [0.5, None, -1.5, 100.125, 0.001]),
    ("s",   pa.string(),                   ["", None, "abc", "h\u00e9llo", "x"]),
    ("ls",  pa.large_string(),             ["", None, "abc", "\u65e5\u672c", "y"]),
    ("bin", pa.binary(),                   [b"", None, b"\x00\x01", b"ab", b"\xff"]),
    ("d",   pa.date32(),                   [0, None, 18262, -1, 1]),
    ("ts",  pa.timestamp("us", tz="UTC"),  [0, None, 1577836800000000, -1, 1]),
    ("dur", pa.duration("us"),             [0, None, 86400000000, -1, 1]),
    ("li",  pa.list_(pa.int64()),          [[], None, [1, 2, 3], [4], [5, 6]]),
]


def mk(case):
    """Build the fixture. case: full | one | empty | allnull."""
    arrays, names = [], []
    for name, typ, vals in COLUMNS:
        if case == "full":
            v = vals
        elif case == "one":
            v = vals[:1]
        elif case == "empty":
            v = []
        elif case == "allnull":
            v = [None] * 5
        else:
            raise ValueError("unknown case " + case)
        arrays.append(pa.array(v, type=typ))
        names.append(name)
    return pa.RecordBatch.from_arrays(arrays, names=names)


def _esc(bs):
    out = []
    for c in bs:
        if 0x20 <= c <= 0x7E and c != 0x5C:
            out.append(chr(c))
        else:
            out.append("\\x%02x" % c)
    return "".join(out)


def _tname(t):
    if pa.types.is_boolean(t): return "bool"
    if pa.types.is_int8(t): return "i8"
    if pa.types.is_int16(t): return "i16"
    if pa.types.is_int32(t): return "i32"
    if pa.types.is_int64(t): return "i64"
    if pa.types.is_uint8(t): return "u8"
    if pa.types.is_uint16(t): return "u16"
    if pa.types.is_uint32(t): return "u32"
    if pa.types.is_uint64(t): return "u64"
    if pa.types.is_float32(t): return "f32"
    if pa.types.is_float64(t): return "f64"
    if pa.types.is_large_string(t): return "large_utf8"
    if pa.types.is_string(t): return "utf8"
    if pa.types.is_binary(t): return "binary"
    if pa.types.is_date32(t): return "date32"
    if pa.types.is_timestamp(t): return "ts_%s_%s" % (t.unit, t.tz)
    if pa.types.is_duration(t): return "dur_%s" % t.unit
    if pa.types.is_list(t): return "list_" + _tname(t.value_type)
    return "?" + str(t)


def _fmt(t, v):
    if v is None:
        return "null"
    if pa.types.is_boolean(t):
        return "true" if v else "false"
    if pa.types.is_floating(t):
        return "%g" % v
    if pa.types.is_string(t) or pa.types.is_large_string(t):
        return _esc(v.encode("utf-8"))
    if pa.types.is_binary(t):
        return _esc(v)
    if pa.types.is_list(t):
        return "[" + ",".join(_fmt(t.value_type, x) for x in v) + "]"
    return str(v)


def describe(rb):
    lines = ["rows=%d cols=%d" % (rb.num_rows, rb.num_columns)]
    for name, col in zip(rb.column_names, rb.columns):
        t = col.type
        if pa.types.is_timestamp(t) or pa.types.is_duration(t):
            vals = col.cast(pa.int64()).to_pylist()
        elif pa.types.is_date32(t):
            vals = col.cast(pa.int32()).to_pylist()
        else:
            vals = col.to_pylist()
        lines.append("%s %s n=%d nulls=%d [%s]" % (
            name, _tname(t), len(col), col.null_count,
            ",".join(_fmt(t, v) for v in vals)))
    return "\n".join(lines)
