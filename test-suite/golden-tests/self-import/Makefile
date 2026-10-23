all:
	rm -f *.err obs.txt
	# Prefer GNU timeout (`timeout`, or `gtimeout` from brew coreutils on macOS);
	# fall back to no bound if neither exists (macOS lacks `timeout` by default).
	@TO=`command -v timeout || command -v gtimeout || true`; \
	$${TO:+$$TO 10} morloc typecheck main.loc > obs.txt 2>&1 || true
	@TO=`command -v timeout || command -v gtimeout || true`; \
	$${TO:+$$TO 10} morloc typecheck a.loc >> obs.txt 2>&1 || true
	@TO=`command -v timeout || command -v gtimeout || true`; \
	$${TO:+$$TO 10} morloc typecheck c.loc >> obs.txt 2>&1 || true

clean:
	rm -f *.err
	rm -rf *-build *build.tmp*
