#!/usr/bin/env bash
# Two binaries, three data shapes, two threshold regimes.
#
# nexus-zero (--inline-size 0): every value -- regardless of size or
# shape -- routes through the FILE branch. 3 commands x 3 pool returns
# per call = 9 files expected.
#
# nexus-thresh (--inline-size 1k): payloads below 1 KiB inline, larger
# payloads route to file. Two calls: one tiny string (should inline,
# 0 files) and one ~1.5 KiB string (should route to file, 3 files).
# Expected count in tmp_thresh: exactly 3.

# === nexus-zero: every transfer to file ============================
echo "--- nexus-zero output ---"
./nexus-zero echoStr    "HELLO_MARKER"
./nexus-zero echoList   '[1,2,3,4,5]'
./nexus-zero echoNested '[[1,2],[3,4]]'

# === nexus-thresh: threshold-driven routing ========================
echo "--- nexus-thresh output ---"
# Small payload (well under 1 KiB serialized): should inline.
./nexus-thresh echoStr "TINY_MARKER"

# Large payload (>1 KiB serialized): should route to file. Construct
# a ~1.5 KiB string with a unique marker so we can grep for it
# without false positives from binary headers.
PAD=$(head -c 1500 /dev/zero | tr '\0' 'x')
LONG="\"LONG_MARKER${PAD}\""
./nexus-thresh echoStr "$LONG" > /dev/null

# === Assertions ====================================================
echo "--- assertions ---"
n_zero=$(ls tmp_zero/morloc-pkt-*.mpk 2>/dev/null | wc -l)
n_thresh=$(ls tmp_thresh/morloc-pkt-*.mpk 2>/dev/null | wc -l)

# (1) Forced-file regime: every call wrote files.
if [ "$n_zero" -ge 9 ]; then
  echo "tmp_zero_files_at_least_9=yes"
else
  echo "tmp_zero_files_at_least_9=no"
fi

# (2) Threshold regime: only the large call wrote files.
#     3 expected; bound from above so we catch the case where the
#     small call also leaked to disk (would mean threshold ignored).
if [ "$n_thresh" -ge 3 ] && [ "$n_thresh" -lt 6 ]; then
  echo "tmp_thresh_files_3_to_5=yes"
else
  echo "tmp_thresh_files_3_to_5=no n=$n_thresh"
fi

# (3) Serialization survives: each data shape's bytes appear in some
#     packet on disk in tmp_zero.
if grep -lF 'HELLO_MARKER' tmp_zero/morloc-pkt-*.mpk > /dev/null 2>&1; then
  echo "Str_serialized=yes"
else
  echo "Str_serialized=no"
fi

# msgpack encodes [1,2,3,4,5] as an array of small positive ints --
# the bytes 0x01 0x02 0x03 0x04 0x05 appear contiguously somewhere.
if find tmp_zero -name 'morloc-pkt-*.mpk' \
   -exec grep -lP '\x01\x02\x03\x04\x05' {} \; 2>/dev/null | grep -q .; then
  echo "List_serialized=yes"
else
  echo "List_serialized=no"
fi

# Nested list [[1,2],[3,4]] msgpack: outer array of 2 inner arrays of
# 2 small ints. The byte sequence "\x01\x02" appears (inner [1,2]).
if find tmp_zero -name 'morloc-pkt-*.mpk' \
   -exec grep -lP '\x01\x02' {} \; 2>/dev/null | grep -q .; then
  echo "Nested_serialized=yes"
else
  echo "Nested_serialized=no"
fi

# (4) Threshold honored: the small "TINY_MARKER" payload must NOT
#     have leaked to disk (proof that the size check skipped it).
if grep -lF 'TINY_MARKER' tmp_thresh/morloc-pkt-*.mpk > /dev/null 2>&1; then
  echo "tiny_in_tmp_thresh=yes_BUG"
else
  echo "tiny_in_tmp_thresh=no"
fi

# (5) Large payload DID hit disk in the threshold regime.
if grep -lF 'LONG_MARKER' tmp_thresh/morloc-pkt-*.mpk > /dev/null 2>&1; then
  echo "long_in_tmp_thresh=yes"
else
  echo "long_in_tmp_thresh=no"
fi
