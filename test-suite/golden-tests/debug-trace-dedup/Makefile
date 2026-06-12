# Content-addressed dedup: same arg content hashes to the same path,
# so passing identical args twice writes ONE .pkt file, not two.
# The rendered trace must show two distinct arg slots both resolving
# to that single file.

all:
	rm -rf *.err logs nexus pools build.err
	@: > obs.txt
	morloc make --debug -o nexus main.loc 2> build.err
	@# --debug-cache-depth 10 lets BOTH args write to disk; dedup
	@# (not depth) is what keeps the count at 1.
	./nexus --log-dir logs --debug-cache-depth 10 \
	    boomP2 7 7 >/dev/null 2> obs.run.err; rc=$$?; \
	  test $$rc -ne 0 \
	  && echo "fail_rc_nonzero: PASS" >> obs.txt \
	  || echo "fail_rc_nonzero: FAIL" >> obs.txt
	@dir=$$(ls -d logs/*/ 2>/dev/null); \
	  cnt=$$(ls $$dir/debug/inputs/*.pkt 2>/dev/null | wc -l); \
	  test $$cnt -eq 1 \
	  && echo "one_pkt_for_two_args: PASS" >> obs.txt \
	  || echo "one_pkt_for_two_args: FAIL (got $$cnt)" >> obs.txt
	@dir=$$(ls -d logs/*/ 2>/dev/null); \
	  pkt=$$(ls $$dir/debug/inputs/*.pkt 2>/dev/null | head -1); \
	  hash=$$(basename $$pkt .pkt); \
	  cnt=$$(grep -o "$$hash\.pkt" $$dir/summary.json | wc -l); \
	  test $$cnt -ge 2 \
	  && echo "both_args_reference_same_pkt: PASS" >> obs.txt \
	  || echo "both_args_reference_same_pkt: FAIL (got $$cnt refs)" >> obs.txt
	cat obs.*.err > obs.err

clean:
	rm -rf nexus pools logs obs.*.err build.err
