all:
	rm -f *.err obs.txt
	for l in cpp rs; do morloc make -o nexus-$$l main-$$l.loc 2>> build.err; done
	for l in cpp rs; do for f in readFirst keepFirst twoKeeps viaAlias; do ./nexus-$$l $$f 5 >> obs.txt 2>> obs.err || echo "$$l $$f failed" >> obs.txt; done; done

clean:
	rm -rf nexus-* pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
