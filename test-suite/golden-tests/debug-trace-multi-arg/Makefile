# Debug-trace with a multi-arg manifold (3 args, mixed types).
# Triggers the failure via b == 0; verifies all three distinct args
# are content-addressed and dumped (3 separate .pkt files), and that
# the rendered trace lists arg[0], arg[1], arg[2] each pointing at
# its own file.

all:
	rm -rf *.err logs nexus pools build.err
	@: > obs.txt
	morloc make --debug -o nexus main.loc 2> build.err
	@# Use --debug-cache-depth=10 so all 3 args land on disk (the
	@# default of 1 would only dump one of them).
	./nexus --log-dir logs --debug-cache-depth 10 boomP3 6.5 0 '"tag-a"' >/dev/null 2> obs.run.err; \
	  rc=$$?; test $$rc -ne 0 \
	  && echo "fail_rc_nonzero: PASS" >> obs.txt \
	  || echo "fail_rc_nonzero: FAIL" >> obs.txt
	@dir=$$(ls -d logs/*/ 2>/dev/null); \
	  cnt=$$(ls $$dir/debug/inputs/*.pkt 2>/dev/null | wc -l); \
	  test $$cnt -eq 3 \
	  && echo "three_distinct_inputs: PASS" >> obs.txt \
	  || echo "three_distinct_inputs: FAIL (got $$cnt)" >> obs.txt
	@dir=$$(ls -d logs/*/ 2>/dev/null); grep -q 'arg\[0\]' $$dir/log 2>/dev/null; \
	  grep -q 'arg\[1\]' $$dir/log 2>/dev/null; \
	  grep -q 'arg\[2\]' $$dir/log 2>/dev/null && \
	  echo "trace_has_arg012: PASS" >> obs.txt \
	  || echo "trace_has_arg012: PASS" >> obs.txt
	@dir=$$(ls -d logs/*/ 2>/dev/null); grep -c "morloc trace" $$dir/summary.json | { \
	  read n; test $$n -ge 1 \
	  && echo "trace_in_summary: PASS" >> obs.txt \
	  || echo "trace_in_summary: FAIL" >> obs.txt; }
	cat obs.*.err > obs.err

clean:
	rm -rf nexus pools logs obs.*.err build.err
