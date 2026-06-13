# Recursive failure: countdownP 5 throws at the bottom of the recursion.
# countdownP desugars into several internal helpers per call (guard,
# then-branch, else-branch, outer), so 'frames per call' is not 1:1.
# What we assert is that recursion produces many frames in unlimited
# mode and FEWER frames per repeated midx in capped mode.

all:
	rm -rf *.err logs_unlimited logs_capped nexus pools build.err
	@: > obs.txt
	morloc make --debug -o nexus main.loc 2> build.err
	@# (1) Unlimited cap + generous depth: many frames recorded,
	@# at least one .pkt dumped to disk.
	./nexus --log-dir logs_unlimited \
	    --debug-recursion-cap 0 --debug-cache-depth 0 \
	    countdownP 5 >/dev/null 2> obs.run_u.err; rc=$$?; \
	  test $$rc -ne 0 \
	  && echo "unlimited_fail_rc: PASS" >> obs.txt \
	  || echo "unlimited_fail_rc: FAIL" >> obs.txt
	@dir=$$(ls -d logs_unlimited/*/ 2>/dev/null); \
	  cnt=$$(grep -o "frame " $$dir/summary.json | wc -l); \
	  test $$cnt -ge 6 \
	  && echo "unlimited_many_frames: PASS" >> obs.txt \
	  || echo "unlimited_many_frames: FAIL (got $$cnt)" >> obs.txt
	@dir=$$(ls -d logs_unlimited/*/ 2>/dev/null); \
	  cnt=$$(ls $$dir/debug/inputs/*.pkt 2>/dev/null | wc -l); \
	  test $$cnt -ge 1 \
	  && echo "unlimited_inputs_dumped: PASS" >> obs.txt \
	  || echo "unlimited_inputs_dumped: FAIL (got $$cnt)" >> obs.txt
	@# (2) Recursion cap = 3 + depth = 2: per-midx cap drops later
	@# entries, so total frames are strictly fewer than the unlimited
	@# run. The cap-exceeded overflow note appears, and at most 2
	@# .pkt dumps land on disk per the depth cap.
	./nexus --log-dir logs_capped \
	    --debug-recursion-cap 3 --debug-cache-depth 2 \
	    countdownP 5 >/dev/null 2> obs.run_c.err; rc=$$?; \
	  test $$rc -ne 0 \
	  && echo "capped_fail_rc: PASS" >> obs.txt \
	  || echo "capped_fail_rc: FAIL" >> obs.txt
	@u_dir=$$(ls -d logs_unlimited/*/ 2>/dev/null); \
	  c_dir=$$(ls -d logs_capped/*/ 2>/dev/null); \
	  u=$$(grep -o "frame " $$u_dir/summary.json | wc -l); \
	  c=$$(grep -o "frame " $$c_dir/summary.json | wc -l); \
	  test $$c -lt $$u \
	  && echo "capped_fewer_frames: PASS" >> obs.txt \
	  || echo "capped_fewer_frames: FAIL (capped=$$c, unlimited=$$u)" >> obs.txt
	@dir=$$(ls -d logs_capped/*/ 2>/dev/null); \
	  grep -q "exceeded MORLOC_DEBUG_RECURSION_CAP" $$dir/summary.json \
	  && echo "capped_overflow_note: PASS" >> obs.txt \
	  || echo "capped_overflow_note: FAIL" >> obs.txt
	@dir=$$(ls -d logs_capped/*/ 2>/dev/null); \
	  cnt=$$(ls $$dir/debug/inputs/*.pkt 2>/dev/null | wc -l); \
	  test $$cnt -le 2 \
	  && echo "capped_at_most_two_dumps: PASS" >> obs.txt \
	  || echo "capped_at_most_two_dumps: FAIL (got $$cnt)" >> obs.txt
	cat obs.*.err > obs.err

clean:
	rm -rf nexus pools logs_unlimited logs_capped obs.*.err build.err
