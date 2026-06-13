# Cross-pool failure: Python dispatches to C++; C++ throws; both
# pools' catches contribute frames to the rendered trace.

all:
	rm -rf *.err logs nexus pools build.err
	@: > obs.txt
	morloc make --debug -o nexus main.loc 2> build.err
	@# Bump cap so both pools' dumps land on disk.
	./nexus --log-dir logs --debug-cache-depth 4 outerP 0 >/dev/null 2> obs.run.err; \
	  rc=$$?; test $$rc -ne 0 \
	  && echo "fail_rc_nonzero: PASS" >> obs.txt \
	  || echo "fail_rc_nonzero: FAIL" >> obs.txt
	@dir=$$(ls -d logs/*/ 2>/dev/null); grep -q "morloc trace" $$dir/summary.json \
	  && echo "trace_in_summary: PASS" >> obs.txt \
	  || echo "trace_in_summary: FAIL" >> obs.txt
	@dir=$$(ls -d logs/*/ 2>/dev/null); \
	  cnt=$$(grep -o "frame " $$dir/summary.json | wc -l); \
	  test $$cnt -ge 2 \
	  && echo "trace_has_multiple_frames: PASS" >> obs.txt \
	  || echo "trace_has_multiple_frames: FAIL (got $$cnt)" >> obs.txt
	@dir=$$(ls -d logs/*/ 2>/dev/null); \
	  cnt=$$(ls $$dir/debug/inputs/*.pkt 2>/dev/null | wc -l); \
	  test $$cnt -ge 1 \
	  && echo "inputs_dumped: PASS" >> obs.txt \
	  || echo "inputs_dumped: FAIL (got $$cnt)" >> obs.txt
	cat obs.*.err > obs.err

clean:
	rm -rf nexus pools logs obs.*.err build.err
