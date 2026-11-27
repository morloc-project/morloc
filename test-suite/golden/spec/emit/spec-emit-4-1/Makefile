all:
	rm -f *.err obs.txt
	for l in py r cpp rs; do morloc make -o nexus-$$l main-$$l.loc 2>> build.err; done
	for l in py r cpp rs; do \
	  for a in "allAbove 0" "allAbove -100" "anyOf 59" "anyOf 60"; do \
	    printf '%s %s: ' $$l "$$a" >> obs.txt; ./nexus-$$l $$a >> obs.txt 2>> obs.err || echo "failed" >> obs.txt; \
	  done; \
	done

clean:
	rm -rf nexus-* pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
