all:
	rm -f *.err obs.txt
	# This test EXPECTS the typecheck to fail. morloc emits the
	# diagnostic on stdout (and exits non-zero); we capture both
	# streams. A successful (exit-0) run is itself a test failure
	# because the missing-constraint diagnostic must fire.
	if morloc typecheck main.loc > obs.txt 2>&1; then \
	  echo "ERROR: typecheck unexpectedly succeeded" > obs.txt; \
	  exit 1; \
	fi

clean:
	rm -f *.err
