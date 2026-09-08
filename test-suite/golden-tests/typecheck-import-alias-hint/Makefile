all:
	rm -f *.err
	: > obs.txt
	# Importing a term does not import the aliases its signature is written
	# in, so the call fails at every use with nothing naming the cause.
	if morloc typecheck missing_alias.loc 2>&1 | grep -qF 'does not bring in the type aliases'; then \
	  echo "OK: alias hint" >> obs.txt; else echo "FAIL: alias hint missing" >> obs.txt; fi
	# Naming the alias too is the fix the hint points at.
	if morloc typecheck with_alias.loc > /dev/null 2>&1; then \
	  echo "OK: alias imported" >> obs.txt; else echo "FAIL: alias import rejected" >> obs.txt; fi
	# A mismatch between two primitives is not an import problem.
	if morloc typecheck plain_mismatch.loc 2>&1 | grep -qF 'does not bring in the type aliases'; then \
	  echo "FAIL: hint fired on a plain mismatch" >> obs.txt; else echo "OK: no hint on plain mismatch" >> obs.txt; fi

clean:
	rm -rf *.err
	rm -rf *-build *build.tmp*
