INPUT_FAIL := 0
INPUT_OK := 1

all:
	rm -rf *.err logs_dbg_* logs_ok_* logs_nodbg nexus_debug nexus_plain pools build*.err
	@: > obs.txt
	@# Build WITH --debug.
	morloc make --debug -o nexus_debug main.loc 2> build_debug.err
	$(MAKE) -s test_lang LANG=P CMD=boomP INPUT=$(INPUT_FAIL) OK_INPUT=$(INPUT_OK)
	$(MAKE) -s test_lang LANG=C CMD=boomC INPUT=$(INPUT_FAIL) OK_INPUT=$(INPUT_OK)
	$(MAKE) -s test_lang LANG=R CMD=boomR INPUT=$(INPUT_FAIL) OK_INPUT=$(INPUT_OK)
	@# Build WITHOUT --debug, confirm no morloc trace.
	morloc make -o nexus_plain main.loc 2> build_plain.err
	./nexus_plain --log-dir logs_nodbg @ boomP $(INPUT_FAIL) >/dev/null 2> obs.nodbg.err; rc=$$?; \
	  test $$rc -ne 0 \
	  && echo "nodbg_fail_rc: PASS" >> obs.txt \
	  || echo "nodbg_fail_rc: FAIL" >> obs.txt
	@dir=$$(ls -d logs_nodbg/*/ 2>/dev/null); grep -q "morloc trace" $$dir/summary.json \
	  && echo "nodbg_no_trace: FAIL" >> obs.txt \
	  || echo "nodbg_no_trace: PASS" >> obs.txt
	@dir=$$(ls -d logs_nodbg/*/ 2>/dev/null); test ! -d $$dir/debug \
	  && echo "nodbg_no_debug_dir: PASS" >> obs.txt \
	  || echo "nodbg_no_debug_dir: FAIL" >> obs.txt
	cat obs.*.err > obs.err

# Per-language assertions. LANG = P/C/R; CMD is the failing export;
# OK_CMD is derived as the matching ok* variant.
test_lang:
	@./nexus_debug --log-dir logs_dbg_$(LANG) @ $(CMD) $(INPUT) >/dev/null 2> obs.fail_$(LANG).err; \
	  rc=$$?; test $$rc -ne 0 \
	  && echo "fail_$(LANG)_rc_nonzero: PASS" >> obs.txt \
	  || echo "fail_$(LANG)_rc_nonzero: FAIL" >> obs.txt
	@dir=$$(ls -d logs_dbg_$(LANG)/*/ 2>/dev/null); test -f $$dir/summary.json \
	  && echo "fail_$(LANG)_summary_exists: PASS" >> obs.txt \
	  || echo "fail_$(LANG)_summary_exists: FAIL" >> obs.txt
	@dir=$$(ls -d logs_dbg_$(LANG)/*/ 2>/dev/null); grep -q '"status": "fail"' $$dir/summary.json \
	  && echo "fail_$(LANG)_status: PASS" >> obs.txt \
	  || echo "fail_$(LANG)_status: FAIL" >> obs.txt
	@dir=$$(ls -d logs_dbg_$(LANG)/*/ 2>/dev/null); grep -q "morloc trace" $$dir/summary.json \
	  && echo "fail_$(LANG)_trace_in_summary: PASS" >> obs.txt \
	  || echo "fail_$(LANG)_trace_in_summary: FAIL" >> obs.txt
	@dir=$$(ls -d logs_dbg_$(LANG)/*/ 2>/dev/null); ls $$dir/debug/inputs/*.pkt >/dev/null 2>&1 \
	  && echo "fail_$(LANG)_inputs_dumped: PASS" >> obs.txt \
	  || echo "fail_$(LANG)_inputs_dumped: FAIL" >> obs.txt
	@# OK run: same compiled-with-debug binary, succeeds, no inputs dir.
	@OK_CMD=$$(echo $(CMD) | sed 's/^boom/ok/'); \
	  ./nexus_debug --log-dir logs_ok_$(LANG) @ $$OK_CMD $(OK_INPUT) >/dev/null 2> obs.ok_$(LANG).err
	@dir=$$(ls -d logs_ok_$(LANG)/*/ 2>/dev/null); grep -q '"status": "ok"' $$dir/summary.json \
	  && echo "ok_$(LANG)_status: PASS" >> obs.txt \
	  || echo "ok_$(LANG)_status: FAIL" >> obs.txt
	@dir=$$(ls -d logs_ok_$(LANG)/*/ 2>/dev/null); test ! -d $$dir/debug/inputs \
	  && echo "ok_$(LANG)_no_inputs_dir: PASS" >> obs.txt \
	  || echo "ok_$(LANG)_no_inputs_dir: FAIL" >> obs.txt

clean:
	rm -rf nexus_debug nexus_plain pools logs_dbg* logs_ok* logs_nodbg \
	  obs.*.err build_debug.err build_plain.err build.err
