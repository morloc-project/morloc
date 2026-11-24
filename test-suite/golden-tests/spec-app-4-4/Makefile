all:
	rm -f *.err obs.txt
	for l in py cpp r rs; do morloc make -o nexus-$$l main-$$l.loc 2>> build.err; done
	for l in py cpp r rs; do \
	  for a in "line1 7" "line3 1 2 3" "commutes" "subtractionCommutes" "banner Bob" "arityOfInc" "arityOfAdd"; do \
	    printf '%s %s: ' $$l "$$a" >> obs.txt; ./nexus-$$l $$a >> obs.txt 2>> obs.err || echo "failed" >> obs.txt; \
	  done; \
	done

clean:
	rm -rf nexus-* pools __pycache__ *.err *-build *build.tmp*
