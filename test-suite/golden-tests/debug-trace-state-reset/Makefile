# Per-dispatch state reset: two sequential nexus invocations of the
# same failing command must produce IDENTICAL trace lengths and dump
# counts. If FRAMES / DUMPED_COUNT / MIDX_COUNTERS ever bled across
# dispatches (e.g. someone added a persistent backing store), the
# second run would accumulate on top of the first.
#
# Per-process state is reset by process death today; this test would
# also catch a regression that moved state to /dev/shm or any other
# cross-process container. The same property is what
# morloc_debug_flush_dispatch guarantees for daemon-mode dispatches.

all:
	rm -rf *.err logs1 logs2 nexus pools build.err
	@: > obs.txt
	morloc make --debug -o nexus main.loc 2> build.err
	./nexus --log-dir logs1 --debug-cache-depth 10 \
	    boomP 0 >/dev/null 2> obs.run_1.err; rc=$$?; \
	  test $$rc -ne 0 \
	  && echo "run1_fail_rc: PASS" >> obs.txt \
	  || echo "run1_fail_rc: FAIL" >> obs.txt
	./nexus --log-dir logs2 --debug-cache-depth 10 \
	    boomP 0 >/dev/null 2> obs.run_2.err; rc=$$?; \
	  test $$rc -ne 0 \
	  && echo "run2_fail_rc: PASS" >> obs.txt \
	  || echo "run2_fail_rc: FAIL" >> obs.txt
	@d1=$$(ls -d logs1/*/ 2>/dev/null); \
	 d2=$$(ls -d logs2/*/ 2>/dev/null); \
	 f1=$$(grep -o "frame " $$d1/summary.json | wc -l); \
	 f2=$$(grep -o "frame " $$d2/summary.json | wc -l); \
	 test $$f1 -gt 0 -a $$f1 -eq $$f2 \
	   && echo "equal_frame_counts: PASS" >> obs.txt \
	   || echo "equal_frame_counts: FAIL (run1=$$f1 run2=$$f2)" >> obs.txt
	@d1=$$(ls -d logs1/*/ 2>/dev/null); \
	 d2=$$(ls -d logs2/*/ 2>/dev/null); \
	 p1=$$(ls $$d1/debug/inputs/*.pkt 2>/dev/null | wc -l); \
	 p2=$$(ls $$d2/debug/inputs/*.pkt 2>/dev/null | wc -l); \
	 test $$p1 -eq $$p2 \
	   && echo "equal_pkt_counts: PASS" >> obs.txt \
	   || echo "equal_pkt_counts: FAIL (run1=$$p1 run2=$$p2)" >> obs.txt
	cat obs.*.err > obs.err

clean:
	rm -rf nexus pools logs1 logs2 obs.*.err build.err
