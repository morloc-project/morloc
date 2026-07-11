# Strongest correctness test: prove the dumped .pkt files actually
# round-trip to the original input value through the morloc
# deserialization path. For each of four type shapes (Int, Real,
# [Int], (Int, Str)) we crash with a known input, locate the dumped
# .pkt, then call the matching @load function against it. The
# loaded value must equal the literal originally passed in.
#
# Using @load (not an external decoder) keeps the test honest about
# the actual user-facing replay flow and robust against future
# changes to the on-disk byte format.

all:
	rm -rf *.err logs_int logs_real logs_list logs_tup nexus pools build.err
	@: > obs.txt
	morloc make --debug -o nexus main.loc 2> build.err
	@# (1) Int round-trip: crash with 42, load the .pkt, assert "42".
	-./nexus --log-dir logs_int @ boomInt 42 >/dev/null 2> obs.run_int.err
	@pkt=$$(ls logs_int/*/debug/inputs/*.pkt 2>/dev/null | head -1); \
	  out=$$(./nexus loadInt "$$pkt" 2> obs.load_int.err); \
	  test "$$out" = "42" \
	  && echo "replay_int: PASS" >> obs.txt \
	  || echo "replay_int: FAIL (got '$$out')" >> obs.txt
	@# (2) Real round-trip.
	-./nexus --log-dir logs_real @ boomReal 3.14 >/dev/null 2> obs.run_real.err
	@pkt=$$(ls logs_real/*/debug/inputs/*.pkt 2>/dev/null | head -1); \
	  out=$$(./nexus loadReal "$$pkt" 2> obs.load_real.err); \
	  test "$$out" = "3.14" \
	  && echo "replay_real: PASS" >> obs.txt \
	  || echo "replay_real: FAIL (got '$$out')" >> obs.txt
	@# (3) [Int] round-trip.
	-./nexus --log-dir logs_list @ boomList '[1,2,3,5,8]' >/dev/null 2> obs.run_list.err
	@pkt=$$(ls logs_list/*/debug/inputs/*.pkt 2>/dev/null | head -1); \
	  out=$$(./nexus loadList "$$pkt" 2> obs.load_list.err); \
	  test "$$out" = "[1,2,3,5,8]" \
	  && echo "replay_list: PASS" >> obs.txt \
	  || echo "replay_list: FAIL (got '$$out')" >> obs.txt
	@# (4) (Int, Str) round-trip.
	-./nexus --log-dir logs_tup @ boomTup '[7,"seven"]' >/dev/null 2> obs.run_tup.err
	@pkt=$$(ls logs_tup/*/debug/inputs/*.pkt 2>/dev/null | head -1); \
	  out=$$(./nexus loadTup "$$pkt" 2> obs.load_tup.err); \
	  test "$$out" = '[7,"seven"]' \
	  && echo "replay_tup: PASS" >> obs.txt \
	  || echo "replay_tup: FAIL (got '$$out')" >> obs.txt
	cat obs.*.err > obs.err

clean:
	rm -rf nexus pools logs_int logs_real logs_list logs_tup obs.*.err build.err
