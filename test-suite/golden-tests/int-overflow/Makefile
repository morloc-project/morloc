# Tag conventions for error-expected tests:
#   OVERFLOW     - value out of range / NA / BigInt does not fit
#   NOT_INTEGER  - JSON value is not a syntactic integer (1.5, 1e2)
#   NA_REJECTED  - explicit R NA propagated to an integer slot
# Pattern: `>> obs.txt 2>/dev/null || echo "TAG" >> obs.txt`
#   - legitimate stdout still appears in obs.txt (catches silent wraps)
#   - non-zero exit produces a stable tag, decoupling tests from
#     verbose error messages whose source line numbers drift as
#     the runtime is edited.
all:
	rm -f *.err obs.txt
	echo "=== S1: compile-time literal errors (negative tests) ===" >> obs.txt
	morloc make bad-sign.loc >> obs.txt 2>> obs.txt || true
	morloc make too-big.loc >> obs.txt 2>> obs.txt || true
	morloc make -o nexus main.loc 2>> build.err
	echo "" >> obs.txt
	echo "=== S2: BigInt round-trip (pure morloc) ===" >> obs.txt
	./nexus bigVarInt >> obs.txt
	./nexus bigVarIntNeg >> obs.txt
	./nexus intId 123456789012345678901234567890 >> obs.txt
	./nexus intId -123456789012345678901234567890 >> obs.txt
	echo "" >> obs.txt
	echo "=== S3: list round-trips ===" >> obs.txt
	./nexus uint8ListId [0,255] >> obs.txt
	./nexus uint8ListId [0,256] >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus intListId [123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890,255,-123456789012345678901234567890] >> obs.txt
	echo "" >> obs.txt
	echo "=== S4: signed identity, positive boundary ===" >> obs.txt
	./nexus int8Id 127 >> obs.txt
	./nexus int8Id 128 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus int16Id 32767 >> obs.txt
	./nexus int16Id 32768 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus int32Id 2147483647 >> obs.txt
	./nexus int32Id 2147483648 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus int64Id 9223372036854775807 >> obs.txt
	./nexus int64Id 9223372036854775808 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	echo "" >> obs.txt
	echo "=== S5: unsigned identity, positive boundary ===" >> obs.txt
	./nexus uint8Id 255 >> obs.txt
	./nexus uint8Id 256 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus uint16Id 65535 >> obs.txt
	./nexus uint16Id 65536 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus uint32Id 4294967295 >> obs.txt
	./nexus uint32Id 4294967296 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus uint64Id 18446744073709551615 >> obs.txt
	./nexus uint64Id 18446744073709551616 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	echo "" >> obs.txt
	echo "=== S6: python doubling at fixed-width boundaries ===" >> obs.txt
	./nexus pintDbl 100000000000000000000000000000 >> obs.txt
	./nexus pint8Dbl 63 >> obs.txt
	./nexus pint8Dbl 64 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus pint16Dbl 16383 >> obs.txt
	./nexus pint16Dbl 16384 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus pint32Dbl 1073741823 >> obs.txt
	./nexus pint32Dbl 1073741824 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus pint64Dbl 4611686018427387903 >> obs.txt
	./nexus pint64Dbl 4611686018427387904 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus puint8Dbl 127 >> obs.txt
	./nexus puint8Dbl 128 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus puint16Dbl 32767 >> obs.txt
	./nexus puint16Dbl 32768 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus puint32Dbl 2147483647 >> obs.txt
	./nexus puint32Dbl 2147483648 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus puint64Dbl 9223372036854775807 >> obs.txt
	./nexus puint64Dbl 9223372036854775808 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	echo "" >> obs.txt
	echo "=== S7: cpp doubling at fixed-width boundaries ===" >> obs.txt
	# C++ has no native overflow detection on fixed-width arithmetic
	# (the wrap-around is indistinguishable from a legal value by the
	# time the wire layer sees it), so the half-max+1 cases are not
	# tested here. They would silently wrap and pass; preventing this
	# is the responsibility of the C++ source code, not morloc.
	# BigInt -> C++ default int IS catchable at unpack time, so it stays.
	# Same path catches the single-limb-fits-i64 cases that overflow int32,
	# in both directions.
	./nexus cintDbl 100000000000000000000000000000 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus cintDbl 2147483648 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus cintDbl -2147483649 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	# Python -> wire -> C++ chain: Python preserves the BigInt on the wire,
	# C++ must reject the value at unpack rather than silently truncate.
	./nexus pintToCint 2147483647 >> obs.txt
	./nexus pintToCint -2147483648 >> obs.txt
	./nexus pintToCint 2147483648 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus pintToCint -2147483649 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus pintToCint 123456789012345678901234567890 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus pintToCint -123456789012345678901234567890 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus cint8Dbl 63 >> obs.txt
	./nexus cint16Dbl 16383 >> obs.txt
	./nexus cint32Dbl 1073741823 >> obs.txt
	./nexus cint64Dbl 4611686018427387903 >> obs.txt
	./nexus cuint8Dbl 127 >> obs.txt
	./nexus cuint16Dbl 32767 >> obs.txt
	./nexus cuint32Dbl 2147483647 >> obs.txt
	./nexus cuint64Dbl 9223372036854775807 >> obs.txt
	echo "" >> obs.txt
	echo "=== S8: r doubling at fixed-width boundaries ===" >> obs.txt
	./nexus rintDbl 100000000000000000000000000000 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus rint8Dbl 63 >> obs.txt
	./nexus rint8Dbl 64 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus rint16Dbl 16383 >> obs.txt
	./nexus rint16Dbl 16384 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus rint32Dbl 1073741823 >> obs.txt
	./nexus rint32Dbl 1073741824 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	# rint64Dbl: R now uses bit64::integer64 for I64, which covers the
	# full 2^63 range losslessly. 2^52 and 2^52+1 both round-trip; their
	# doubled values (2^53 and 2^53+2) sit comfortably below I64 max.
	# (R-side arithmetic on integer64 wraps silently on I64 overflow --
	# matching C int64_t -fwrapv semantics -- so an explicit doubling
	# OVERFLOW boundary near 2^63 is not catchable post-hoc.)
	./nexus rint64Dbl 4503599627370496 >> obs.txt
	./nexus rint64Dbl 4503599627370497 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus ruint8Dbl 127 >> obs.txt
	./nexus ruint8Dbl 128 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus ruint16Dbl 32767 >> obs.txt
	./nexus ruint16Dbl 32768 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus ruint32Dbl 2147483647 >> obs.txt
	./nexus ruint32Dbl 2147483648 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	# ruint64Dbl: same bit64 round-trip semantics as rint64Dbl. U64 is
	# carried as integer64 bytes; 2^52+1 doubled is 2^53+2, well within
	# U64. (Arithmetic wrap at 2^64 is not catchable in R, see above.)
	./nexus ruint64Dbl 4503599627370496 >> obs.txt
	./nexus ruint64Dbl 4503599627370497 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	echo "" >> obs.txt
	echo "=== S9: signed identity, negative boundary ===" >> obs.txt
	./nexus int8Id -128 >> obs.txt
	./nexus int8Id -129 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus int16Id -32768 >> obs.txt
	./nexus int16Id -32769 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus int32Id -2147483648 >> obs.txt
	./nexus int32Id -2147483649 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus int64Id -9223372036854775808 >> obs.txt
	./nexus int64Id -9223372036854775809 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	echo "" >> obs.txt
	echo "=== S10: signed doubling, negative-side underflow (Python and R) ===" >> obs.txt
	# C++ excluded: silent wraparound, not catchable post-hoc.
	./nexus pint8Dbl -64 >> obs.txt
	./nexus pint8Dbl -65 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus pint16Dbl -16384 >> obs.txt
	./nexus pint16Dbl -16385 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus pint32Dbl -1073741824 >> obs.txt
	./nexus pint32Dbl -1073741825 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus pint64Dbl -4611686018427387904 >> obs.txt
	./nexus pint64Dbl -4611686018427387905 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus rint8Dbl -64 >> obs.txt
	./nexus rint8Dbl -65 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus rint16Dbl -16384 >> obs.txt
	./nexus rint16Dbl -16385 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus rint32Dbl -1073741824 >> obs.txt
	./nexus rint32Dbl -1073741825 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	# rint64Dbl: with bit64::integer64, -2^52-1 doubles to -2^53-2 which is
	# well within I64 min (-2^63). The full negative I64 range
	# round-trips losslessly; near-I64-min arithmetic wraps silently
	# (not catchable post-hoc).
	./nexus rint64Dbl -4503599627370496 >> obs.txt
	./nexus rint64Dbl -4503599627370497 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	echo "" >> obs.txt
	echo "=== S11: unsigned types reject negative input ===" >> obs.txt
	./nexus uint8Id -1 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus uint16Id -1 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus uint32Id -1 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	./nexus uint64Id -1 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	echo "" >> obs.txt
	echo "=== S12: integer slot rejects float syntax ===" >> obs.txt
	./nexus int8Id 1.5 >> obs.txt 2>/dev/null || echo "NOT_INTEGER" >> obs.txt
	./nexus int8Id 1e2 >> obs.txt 2>/dev/null || echo "NOT_INTEGER" >> obs.txt
	./nexus uint8Id 1.5 >> obs.txt 2>/dev/null || echo "NOT_INTEGER" >> obs.txt
	./nexus intId 1.5 >> obs.txt 2>/dev/null || echo "NOT_INTEGER" >> obs.txt
	./nexus intId 1e2 >> obs.txt 2>/dev/null || echo "NOT_INTEGER" >> obs.txt
	echo "" >> obs.txt
	echo "=== S13: zero and +/-1 sanity ===" >> obs.txt
	./nexus intId 0 >> obs.txt
	./nexus int8Id 0 >> obs.txt
	./nexus int8Id 1 >> obs.txt
	./nexus int8Id -1 >> obs.txt
	./nexus uint8Id 0 >> obs.txt
	./nexus uint8Id 1 >> obs.txt
	./nexus int64Id 0 >> obs.txt
	./nexus uint64Id 0 >> obs.txt
	echo "" >> obs.txt
	echo "=== S14: BigInt boundary values (i64 / u64 / multi-limb transitions) ===" >> obs.txt
	./nexus intId 9223372036854775807 >> obs.txt   # i64 max
	./nexus intId 9223372036854775808 >> obs.txt   # i64 max + 1 (fits u64, single limb)
	./nexus intId 18446744073709551615 >> obs.txt  # u64 max (single limb)
	./nexus intId 18446744073709551616 >> obs.txt  # u64 max + 1 (multi-limb)
	./nexus intId -9223372036854775808 >> obs.txt  # i64 min
	./nexus intId -9223372036854775809 >> obs.txt  # just below i64 min (multi-limb)
	./nexus intId -18446744073709551615 >> obs.txt # -u64 max (multi-limb)
	./nexus intId -18446744073709551616 >> obs.txt # -2^64 (deeper multi-limb)
	echo "" >> obs.txt
	echo "=== S15: R narrow numeric (round-trip identity through R) ===" >> obs.txt
	# I8/I16 and U8/U16 fit R's 32-bit "integer".
	# I32 and U32 map to R's "numeric" (double); values up to 2^53
	# round-trip exactly.
	# I64 and U64 map to bit64::integer64, which uses 8 bytes per
	# element and round-trips the full 64-bit range losslessly. (The
	# Int instance -- variable-precision BigInt -- still maps to R's
	# numeric, so values beyond 2^53 there still surface as OVERFLOW.)
	./nexus rint8Id 127 >> obs.txt
	./nexus rint8Id -128 >> obs.txt
	./nexus rint16Id 32767 >> obs.txt
	./nexus rint16Id -32768 >> obs.txt
	./nexus rint32Id 2147483647 >> obs.txt
	./nexus rint32Id -2147483648 >> obs.txt
	./nexus ruint8Id 255 >> obs.txt
	./nexus ruint16Id 65535 >> obs.txt
	./nexus ruint32Id 4294967295 >> obs.txt   # fits 2^53, should round-trip
	./nexus rint64Id 9007199254740992 >> obs.txt   # 2^53 (was last losslessly representable under double; still round-trips)
	./nexus rint64Id 9007199254740993 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt   # 2^53 + 1 (bit64 round-trips losslessly)
	./nexus rint64Id 9223372036854775807 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt   # I64 max (bit64 round-trips losslessly)
	./nexus rint64Id -9223372036854775808 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt  # I64 min (bit-identical even at NA_INTEGER64 collision; bytes survive the idr round-trip)
	./nexus ruint64Id 18446744073709551615 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt # U64 max (carried as -1 in bit64's signed view; bits round-trip)
	./nexus rintId 100000000000000000000000000000 >> obs.txt 2>/dev/null || echo "OVERFLOW" >> obs.txt
	echo "" >> obs.txt
	echo "=== S16: R NA rejection on integer pack ===" >> obs.txt
	./nexus returnNAI8 0 >> obs.txt 2>/dev/null || echo "NA_REJECTED" >> obs.txt


clean:
	rm -rf nexus nexus_bad pools *err __pycache__
