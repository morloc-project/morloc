# Run-scope log + summary.json across activation modes.
#
# The test exercises:
#   * Default (no flags): NO rundir/summary.json/log lines anywhere.
#   * --log-dir PATH: rundir created, log file teed, summary.json
#     written with the ok-branch fields.
#   * --log-dir PATH on a failing run: fail-branch epilogue, summary
#     status=fail with exit_code=1 and a populated error field.
#   * --summary FILE alone: summary.json at a custom path, no rundir.
#   * --quiet --log-dir PATH: no log lines, summary.json still written
#     (quiet is for noise suppression; sentinel survives).
#
# obs.txt records assertions, not raw output, so the test is stable
# across timestamp / run_id / pid drift.

all:
	rm -rf *.err obs.txt logs_ok logs_fail summary-only.json logs_quiet
	morloc make -o nexus main.loc 2> build.err
	@# (1) Default (no flags): prologue/epilogue still fire on stderr
	@# (they are part of "logging", which is always-on whenever templates
	@# are declared in the YAML); but NO rundir / log file / summary.json
	@# is materialized -- those are gated behind --log-dir.
	./nexus greet 1 >/dev/null 2> obs.default.err
	@test ! -d logs_ok && echo "default_no_rundir: PASS" >> obs.txt \
	  || echo "default_no_rundir: FAIL" >> obs.txt
	@grep -q "prologue name=greet" obs.default.err \
	  && echo "default_prologue_on_stderr: PASS" >> obs.txt \
	  || echo "default_prologue_on_stderr: FAIL" >> obs.txt
	@grep -q "epilogue.ok name=greet" obs.default.err \
	  && echo "default_epilogue_on_stderr: PASS" >> obs.txt \
	  || echo "default_epilogue_on_stderr: FAIL" >> obs.txt
	@# (2) --log-dir on ok run.
	./nexus --log-dir logs_ok greet 1 >/dev/null 2> obs.ok.err
	@dir=$$(ls -d logs_ok/*/); test -f $$dir/summary.json \
	  && echo "ok_summary_exists: PASS" >> obs.txt \
	  || echo "ok_summary_exists: FAIL" >> obs.txt
	@dir=$$(ls -d logs_ok/*/); grep -q '"status": "ok"' $$dir/summary.json \
	  && echo "ok_status: PASS" >> obs.txt \
	  || echo "ok_status: FAIL" >> obs.txt
	@dir=$$(ls -d logs_ok/*/); grep -q '"command": "greet"' $$dir/summary.json \
	  && echo "ok_command: PASS" >> obs.txt \
	  || echo "ok_command: FAIL" >> obs.txt
	@dir=$$(ls -d logs_ok/*/); grep -q '"morloc_version"' $$dir/summary.json \
	  && echo "ok_morloc_version: PASS" >> obs.txt \
	  || echo "ok_morloc_version: FAIL" >> obs.txt
	@dir=$$(ls -d logs_ok/*/); test -f $$dir/log \
	  && echo "ok_log_exists: PASS" >> obs.txt \
	  || echo "ok_log_exists: FAIL" >> obs.txt
	@dir=$$(ls -d logs_ok/*/); grep -q "prologue name=greet" $$dir/log \
	  && echo "ok_log_prologue: PASS" >> obs.txt \
	  || echo "ok_log_prologue: FAIL" >> obs.txt
	@dir=$$(ls -d logs_ok/*/); grep -q "epilogue.ok name=greet" $$dir/log \
	  && echo "ok_log_epilogue: PASS" >> obs.txt \
	  || echo "ok_log_epilogue: FAIL" >> obs.txt
	@grep -q "prologue name=greet" obs.ok.err \
	  && echo "ok_stderr_prologue: PASS" >> obs.txt \
	  || echo "ok_stderr_prologue: FAIL" >> obs.txt
	@# (3) --log-dir on failing run (foreign Python ZeroDivisionError).
	./nexus --log-dir logs_fail boomP 0 >/dev/null 2> obs.fail.err; rc=$$?; \
	  echo "fail_rc=$$rc"; test $$rc -ne 0 \
	  && echo "fail_rc_nonzero: PASS" >> obs.txt \
	  || echo "fail_rc_nonzero: FAIL" >> obs.txt
	@dir=$$(ls -d logs_fail/*/); grep -q '"status": "fail"' $$dir/summary.json \
	  && echo "fail_status: PASS" >> obs.txt \
	  || echo "fail_status: FAIL" >> obs.txt
	@dir=$$(ls -d logs_fail/*/); grep -q '"exit_code": 1' $$dir/summary.json \
	  && echo "fail_exit_code: PASS" >> obs.txt \
	  || echo "fail_exit_code: FAIL" >> obs.txt
	@dir=$$(ls -d logs_fail/*/); grep -q '"error":' $$dir/summary.json \
	  && grep -q 'ZeroDivisionError\|division\|divide' $$dir/summary.json \
	  && echo "fail_error_captured: PASS" >> obs.txt \
	  || echo "fail_error_captured: FAIL" >> obs.txt
	@dir=$$(ls -d logs_fail/*/); grep -q "epilogue.fail" $$dir/log \
	  && echo "fail_log_epilogue: PASS" >> obs.txt \
	  || echo "fail_log_epilogue: FAIL" >> obs.txt
	@# (4) --summary alone: no rundir, just the sentinel file.
	./nexus --summary $$PWD/summary-only.json greet 1 >/dev/null 2> obs.solo.err
	@test -f summary-only.json \
	  && echo "solo_summary_exists: PASS" >> obs.txt \
	  || echo "solo_summary_exists: FAIL" >> obs.txt
	@grep -q '"status": "ok"' summary-only.json \
	  && echo "solo_status: PASS" >> obs.txt \
	  || echo "solo_status: FAIL" >> obs.txt
	@# (5) --quiet --log-dir: no stderr log lines, summary.json still landed.
	./nexus --quiet --log-dir logs_quiet greet 1 >/dev/null 2> obs.quiet.err
	@grep -qE "prologue|epilogue" obs.quiet.err \
	  && echo "quiet_no_stderr_lines: FAIL" >> obs.txt \
	  || echo "quiet_no_stderr_lines: PASS" >> obs.txt
	@dir=$$(ls -d logs_quiet/*/); test -f $$dir/summary.json \
	  && echo "quiet_summary_exists: PASS" >> obs.txt \
	  || echo "quiet_summary_exists: FAIL" >> obs.txt
	@dir=$$(ls -d logs_quiet/*/); test ! -f $$dir/log \
	  && echo "quiet_no_log_file: PASS" >> obs.txt \
	  || echo "quiet_no_log_file: FAIL" >> obs.txt
	cat obs.*.err > obs.err

clean:
	rm -rf nexus pools logs_ok logs_fail logs_quiet summary-only.json obs.*.err build.err
