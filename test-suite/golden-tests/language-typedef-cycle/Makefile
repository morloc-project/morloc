# Each cycle variant must be detected by the frontend. Pre-fix every
# case hangs; the timeout prevents the test suite from wedging. Post-fix
# every case emits "Mutual recursion ... type definitions" with a clean
# caret and the program is rejected.
#
# Cross-module language-specific cycles are not tested here: they
# require a circular import between modules, which morloc's
# resolveImports rejects with its own "Cyclical import dependency"
# error before the typedef-cycle check would fire. So the reachable
# surface for the language-specific cycle hang is intra-module only.
all:
	rm -f *.err obs.txt
	# Intra-module Cpp scope cycle.
	-timeout 10s morloc make -o nexus_a intra-cpp.loc 2> intra-cpp.err
	@if grep -q "Mutual recursion" intra-cpp.err 2>/dev/null; then echo 1; else echo 0; fi >> obs.txt
	# Intra-module Py scope cycle. Verifies per-language SCC: the
	# fix must not conflate Cpp and Py typedefs with the same name.
	-timeout 10s morloc make -o nexus_b intra-py.loc 2> intra-py.err
	@if grep -q "Mutual recursion" intra-py.err 2>/dev/null; then echo 1; else echo 0; fi >> obs.txt
	# Parameterized Cpp cycle. SCC walk must handle aliases with
	# Either-encoded params.
	-timeout 10s morloc make -o nexus_c param-cpp.loc 2> param-cpp.err
	@if grep -q "Mutual recursion" param-cpp.err 2>/dev/null; then echo 1; else echo 0; fi >> obs.txt

clean:
	rm -rf nexus_* pools *err obs.txt
