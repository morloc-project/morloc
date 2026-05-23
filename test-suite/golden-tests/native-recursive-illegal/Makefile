SHELL := /bin/bash

# Each illegal pattern must reject at compile time. We bound the wall
# clock per build with `timeout 5s` so that a divergence regression in
# evaluateType / Restructure surfaces as a test failure rather than a
# hung suite. The token "REJECTED" is written iff morloc make exits
# non-zero; "DIVERGED" is written iff the timeout fires; "UNEXPECTED"
# is written iff the build unexpectedly succeeds.

CASES := bare-field bare-tuple bare-function mutual-2 mutual-3 mutual-guarded mutual-tuple vacuous-identity vacuous-optional vacuous-parametric

all:
	rm -f *.err obs.txt
	@for c in $(CASES); do \
		out=$$(timeout 5s morloc make -o /tmp/nrtest_$$c $$c.loc 2> $$c.err); \
		rc=$$?; \
		if [ $$rc -eq 124 ]; then \
			echo "$$c DIVERGED" >> obs.txt; \
		elif [ $$rc -eq 0 ]; then \
			echo "$$c UNEXPECTED" >> obs.txt; \
		else \
			echo "$$c REJECTED" >> obs.txt; \
		fi; \
		rm -f /tmp/nrtest_$$c /tmp/nrtest_$$c.manifest; \
	done

clean:
	rm -rf /tmp/nrtest_* *.err *.manifest pools __pycache__
