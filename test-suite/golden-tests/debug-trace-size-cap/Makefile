# Per-arg size cap: a non-trivial list's msgpack encoding clearly
# exceeds 10 bytes, so --debug-cache-max=10 must skip the disk write
# and surface the "size exceeded" marker in the rendered frame.

all:
	rm -rf *.err logs nexus pools build.err
	@: > obs.txt
	morloc make --debug -o nexus main.loc 2> build.err
	./nexus --log-dir logs --debug-cache-max 10 \
	    boomP '["alpha","bravo","charlie","delta","echo","foxtrot"]' \
	    >/dev/null 2> obs.run.err; rc=$$?; \
	  test $$rc -ne 0 \
	  && echo "fail_rc_nonzero: PASS" >> obs.txt \
	  || echo "fail_rc_nonzero: FAIL" >> obs.txt
	@dir=$$(ls -d logs/*/ 2>/dev/null); \
	  grep -q "size exceeded MORLOC_DEBUG_CACHE_MAX" $$dir/summary.json \
	  && echo "size_exceeded_message: PASS" >> obs.txt \
	  || echo "size_exceeded_message: FAIL" >> obs.txt
	@dir=$$(ls -d logs/*/ 2>/dev/null); \
	  cnt=$$(ls $$dir/debug/inputs/*.pkt 2>/dev/null | wc -l); \
	  test $$cnt -eq 0 \
	  && echo "no_pkt_written: PASS" >> obs.txt \
	  || echo "no_pkt_written: FAIL (got $$cnt)" >> obs.txt
	cat obs.*.err > obs.err

clean:
	rm -rf nexus pools logs obs.*.err build.err
