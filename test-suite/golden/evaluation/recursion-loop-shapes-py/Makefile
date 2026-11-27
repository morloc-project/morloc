all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	# DIAGNOSTIC (macOS pool-crash capture): run each call continuing on error and
	# record the nexus exit code, so a mid-run pool crash does not abort the recipe
	# and hide later calls. Combined with PYTHONFAULTHANDLER (set in CI), a fatal
	# signal in the Python worker dumps a C+Python traceback into obs.err.
	@for args in "sumWithLet 0 100" "sumWithLet 0 100000" "collatz 27" "collatz 837799"; do \
	  echo "=== ./nexus $$args ===" >> obs.err; \
	  ./nexus $$args >> obs.txt 2>> obs.err; \
	  echo "=== exit=$$? ===" >> obs.err; \
	done
	# Best-effort: dump any macOS crash report written during this run (captures
	# the signal + backtrace for cpp/R workers too, which faulthandler cannot).
	-@find "$$HOME/Library/Logs/DiagnosticReports" -name '*.ips' -newermt '-3 minutes' 2>/dev/null | while read f; do echo "--- crash report: $$f ---" >> obs.err; cat "$$f" >> obs.err 2>/dev/null; done

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
