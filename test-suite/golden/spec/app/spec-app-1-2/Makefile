all:
	rm -f *.err obs.txt
	for l in py cpp; do morloc make -o nexus-$$l main-$$l.loc 2>> build.err; done
	for l in py cpp; do \
	  for a in "go 41" "adds [1,2,3]"; do \
	    printf '%s %s: ' $$l "$$a" >> obs.txt; ./nexus-$$l $$a >> obs.txt 2>> obs.err || echo "failed" >> obs.txt; \
	  done; \
	done

clean:
	rm -rf nexus-* pools __pycache__ *.err *-build *build.tmp*
