# String-NUL skip-check test.
#
# Verifies that MORLOC_SKIP_NULL_CHECK=1 bypasses the runtime NUL guard
# at the cross-pool boundary. The R pool then encounters the NUL itself
# and raises its native "embedded nul in string" error. We assert that:
#   * the guard's specific morloc-level message does NOT appear (proof
#     that the skip flag actually skipped the scan)
#   * a NUL-related error from R DOES appear
#
# This is a behavioural lock on the opt-out path; users who set the
# skip env var are taking responsibility for downstream pool crashes.

all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	>obs.txt
	NULARG=$$(printf '"abc\\u0000def"') ; \
	  MORLOC_SKIP_NULL_CHECK=1 ./nexus rEcho "$$NULARG" >> obs.txt 2>> obs.err && echo 'RC=0' >> obs.txt || echo 'RC<>0' >> obs.txt
	# The morloc guard's message must be absent.
	if grep -F 'does not support embedded NUL bytes' obs.err > /dev/null ; then \
	  echo 'GUARD STILL FIRED (regression)' >> obs.txt ; \
	else \
	  echo 'GUARD SKIPPED' >> obs.txt ; \
	fi
	# Some NUL-related error must still surface from R.
	if grep -i 'nul' obs.err > /dev/null ; then \
	  echo 'NUL ERROR FROM R' >> obs.txt ; \
	else \
	  echo 'NO NUL ERROR' >> obs.txt ; \
	fi

clean:
	rm -rf nexus pools __pycache__
