all:
	rm -f *.err obs.txt
	if morloc typecheck main.loc > build.err 2>&1; then \
	  echo "ERROR: typecheck unexpectedly succeeded" > obs.txt; \
	  exit 1; \
	fi
	grep -c "deriving (Packable)" build.err >> obs.txt || true
	grep -c "signature" build.err >> obs.txt || true

clean:
	rm -f *.err
	rm -rf *-build *build.tmp*
