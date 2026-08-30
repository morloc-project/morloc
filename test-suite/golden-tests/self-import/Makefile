all:
	rm -f *.err obs.txt
	# Bound the run time so a regression to OOM is caught. A module
	# that imports itself must be rejected with a clear cycle
	# diagnostic, never an OOM.
	# Prefer GNU timeout (`timeout`, or `gtimeout` from brew coreutils on macOS);
	# fall back to no bound if neither exists (macOS lacks `timeout` by default).
	@TO=`command -v timeout || command -v gtimeout || true`; \
	$${TO:+$$TO 10} morloc typecheck main.loc > obs.txt 2>&1 || true

clean:
	rm -f *.err
