"""Test workload generator and structural verifier.

The shape mirrors the morloc-side `Entry = (Str, [Real], ?Str, I64)`
nested as `[[Entry]]`. Generation is deterministic in (outer_count,
str_len) so the verifier can compare exact scalar fingerprints across
runs and across languages.
"""


def gen_big(outer_count, str_len):
    """Build outer_count outer lists of ~32 entries each.

    Each entry: (header, list-of-floats, optional-quality, position).
    String fields are sized by `str_len` so callers can sweep packet
    sizes for the size-sweep tier. Returns a Python list shaped to
    `[[Entry]]`.
    """
    rng_a = 1
    rng_b = 1
    out = []
    for i in range(outer_count):
        inner = []
        # Vary the inner group size to break uniformity (helps catch
        # alignment / variable-tail emit bugs).
        group_size = 16 + (i % 32)
        for j in range(group_size):
            # Deterministic "header" with fixed prefix + index pair.
            header = ("h%d.%d_" % (i, j)) + ("a" * str_len)
            reals = [float(k * 13 + 7) for k in range(4 + (j % 8))]
            qual = None if (j % 3 == 0) else (("q" * str_len) + ("%d" % j))
            pos = (rng_a * 1103515245 + 12345) & 0x7FFFFFFFFFFFFFFF
            rng_a = (rng_a + j + 1) & 0xFFFFFFFF
            rng_b = (rng_b * 2 + i + 1) & 0xFFFFFFFF
            inner.append((header, reals, qual, pos))
        out.append(inner)
    return out


def sum_big(bundle):
    """Structural fingerprint of a bundle. Traversal touches every
    variable-bearing relptr in the structure, so corruption surfaces as
    a wrong scalar or a crash inside this traversal.

    Returns (outer_count, sum_of_reals, optional_string_chars, last_int).
    """
    outer_count = len(bundle)
    sum_reals = 0.0
    opt_chars = 0
    last_int = 0
    for group in bundle:
        for entry in group:
            (header, reals, qual, pos) = entry
            # Touch header bytes -- length is enough to force the relptr
            # follow without copying.
            _ = len(header)
            for r in reals:
                sum_reals += r
            if qual is not None:
                opt_chars += len(qual)
            last_int = pos
    return (outer_count, sum_reals, opt_chars, last_int)
