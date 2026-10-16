all:
	rm -f *.err obs.txt
	for l in py r cpp rs; do morloc make -o nexus-$$l main-$$l.loc 2>> build.err; done
	for l in py r cpp rs; do ./nexus-$$l f 0 >> obs.txt 2>> obs.err || echo "$$l failed" >> obs.txt; ./nexus-$$l f 4 >> obs.txt 2>> obs.err || echo "$$l failed" >> obs.txt; done

clean:
	rm -rf nexus-* pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
