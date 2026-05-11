# Tag conventions for error-expected tests:
#   PARSE_ERROR  - JSON literal is not a recognised Real value
# Compile-time literal overflow is a sourced compile error and contributes
# its full (file:line:col) message to obs.txt; the format is stable, so
# the golden captures it verbatim.
#
# IEEE-754 arithmetic note: all three target languages (Python, R, C++) are
# IEEE-754 conformant for the operations exercised here, so per-language
# results should be identical. One known cross-language divergence not
# covered: Python raises ZeroDivisionError on `1.0/0.0` whereas R and C++
# produce +Inf per IEEE-754. Inf is constructed via the `Inf` literal in
# this test, not via division-by-zero, so that divergence is not exercised.
all:
	rm -rf badF64 badNeg badF32 nexus pools obs.txt build.err obs.err __pycache__
	echo "=== S1: compile-time real-literal overflow ===" >> obs.txt
	morloc make bad-overflow-f64.loc >> obs.txt 2>> obs.txt || true
	morloc make bad-neg-overflow.loc >> obs.txt 2>> obs.txt || true
	morloc make bad-overflow-f32.loc >> obs.txt 2>> obs.txt || true
	morloc make -o nexus main.loc 2>> build.err
	echo "" >> obs.txt
	echo "=== S2: pure-morloc Real identity (finite values) ===" >> obs.txt
	./nexus realId 1.5 >> obs.txt
	./nexus realId 0 >> obs.txt
	./nexus realId -3.14 >> obs.txt
	./nexus realId 1.7976931348623157e308 >> obs.txt
	echo "" >> obs.txt
	echo "=== S3: factorial overflow (the report's f 200 example) ===" >> obs.txt
	./nexus fact 10 >> obs.txt
	./nexus fact 170 >> obs.txt
	./nexus fact 171 >> obs.txt
	./nexus fact 200 >> obs.txt
	echo "" >> obs.txt
	echo "=== S4: Python non-finite emission ===" >> obs.txt
	./nexus pyInf 0 >> obs.txt
	./nexus pyNegInf 0 >> obs.txt
	./nexus pyNaN 0 >> obs.txt
	echo "" >> obs.txt
	echo "=== S5: R non-finite emission ===" >> obs.txt
	./nexus rInf 0 >> obs.txt
	./nexus rNegInf 0 >> obs.txt
	./nexus rNaN 0 >> obs.txt
	echo "" >> obs.txt
	echo "=== S6: C++ non-finite emission ===" >> obs.txt
	./nexus cppInf 0 >> obs.txt
	./nexus cppNegInf 0 >> obs.txt
	./nexus cppNaN 0 >> obs.txt
	echo "" >> obs.txt
	echo "=== S7: source-level non-finite literals (Inf / -Inf / NaN) ===" >> obs.txt
	./nexus litInf >> obs.txt
	./nexus litNegInf >> obs.txt
	./nexus litNaN >> obs.txt
	echo "" >> obs.txt
	echo "=== S8: JSON decode of non-finite string forms (CLI -> Real) ===" >> obs.txt
	./nexus realId '"inf"' >> obs.txt
	./nexus realId '"-inf"' >> obs.txt
	./nexus realId '"nan"' >> obs.txt
	./nexus realId '"Infinity"' >> obs.txt
	./nexus realId '"-Infinity"' >> obs.txt
	./nexus realId '"NaN"' >> obs.txt
	./nexus realId '"+inf"' >> obs.txt
	echo "" >> obs.txt
	echo "=== S9: legacy null decode (best-effort recovery -> NaN) ===" >> obs.txt
	./nexus realId 'null' >> obs.txt
	echo "" >> obs.txt
	echo "=== S10: invalid Real strings reject ===" >> obs.txt
	./nexus realId '"hello"' >> obs.txt 2>/dev/null || echo "PARSE_ERROR" >> obs.txt
	./nexus realId '"infinit"' >> obs.txt 2>/dev/null || echo "PARSE_ERROR" >> obs.txt
	echo "" >> obs.txt
	echo "=== S11: Float32 round-trip incl. non-finites ===" >> obs.txt
	./nexus f32Id 3.14 >> obs.txt
	./nexus f32Id '"inf"' >> obs.txt
	./nexus f32Id '"-inf"' >> obs.txt
	./nexus f32Id '"nan"' >> obs.txt
	echo "" >> obs.txt
	echo "=== S12: Python IEEE-754 arithmetic on Inf / -Inf / NaN ===" >> obs.txt
	./nexus pyInfPlusInf >> obs.txt
	./nexus pyInfPlusNegInf >> obs.txt
	./nexus pyInfMinusInf >> obs.txt
	./nexus pyInfTimesZero >> obs.txt
	./nexus pyZeroTimesInf >> obs.txt
	./nexus pyInfTimesTwo >> obs.txt
	./nexus pyInfTimesNegOne >> obs.txt
	./nexus pyInfTimesInf >> obs.txt
	./nexus pyInfTimesNegInf >> obs.txt
	./nexus pyNaNPlusOne >> obs.txt
	./nexus pyNaNTimesZero >> obs.txt
	./nexus pyNaNTimesInf >> obs.txt
	./nexus pyNegOfInf >> obs.txt
	./nexus pyNegOfNaN >> obs.txt
	./nexus pyFiniteOverflow >> obs.txt
	echo "" >> obs.txt
	echo "=== S13: R IEEE-754 arithmetic on Inf / -Inf / NaN ===" >> obs.txt
	./nexus rInfPlusInf >> obs.txt
	./nexus rInfPlusNegInf >> obs.txt
	./nexus rInfMinusInf >> obs.txt
	./nexus rInfTimesZero >> obs.txt
	./nexus rZeroTimesInf >> obs.txt
	./nexus rInfTimesTwo >> obs.txt
	./nexus rInfTimesNegOne >> obs.txt
	./nexus rInfTimesInf >> obs.txt
	./nexus rInfTimesNegInf >> obs.txt
	./nexus rNaNPlusOne >> obs.txt
	./nexus rNaNTimesZero >> obs.txt
	./nexus rNaNTimesInf >> obs.txt
	./nexus rNegOfInf >> obs.txt
	./nexus rNegOfNaN >> obs.txt
	./nexus rFiniteOverflow >> obs.txt
	echo "" >> obs.txt
	echo "=== S14: C++ IEEE-754 arithmetic on Inf / -Inf / NaN ===" >> obs.txt
	./nexus cppInfPlusInf >> obs.txt
	./nexus cppInfPlusNegInf >> obs.txt
	./nexus cppInfMinusInf >> obs.txt
	./nexus cppInfTimesZero >> obs.txt
	./nexus cppZeroTimesInf >> obs.txt
	./nexus cppInfTimesTwo >> obs.txt
	./nexus cppInfTimesNegOne >> obs.txt
	./nexus cppInfTimesInf >> obs.txt
	./nexus cppInfTimesNegInf >> obs.txt
	./nexus cppNaNPlusOne >> obs.txt
	./nexus cppNaNTimesZero >> obs.txt
	./nexus cppNaNTimesInf >> obs.txt
	./nexus cppNegOfInf >> obs.txt
	./nexus cppNegOfNaN >> obs.txt
	./nexus cppFiniteOverflow >> obs.txt

clean:
	rm -rf badF64 badNeg badF32 nexus pools *err __pycache__
