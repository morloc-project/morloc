all:
	rm -f *.err obs.txt
	for l in py cpp r rs; do morloc make -o nexus-$$l main-$$l.loc 2>> build.err; done
	for l in py cpp r rs; do \
	  for a in "srcApply 10" "defApply 10" "viaIf true 10" "viaIf false 10" "viaList 0 10" "viaList 1 10" "curriedApply 10" "curriedPick true 10" "curriedPick false 10"; do \
	    printf '%s %s: ' $$l "$$a" >> obs.txt; ./nexus-$$l $$a >> obs.txt 2>> obs.err || echo "failed" >> obs.txt; \
	  done; \
	done

clean:
	rm -rf nexus-* pools __pycache__ *.err *-build *build.tmp*
