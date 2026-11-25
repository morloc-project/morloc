STRIP = sed -e 's/mid=[0-9]*, //g' -e 's/(pid [0-9]*)//'
all:
	rm -f *.err obs.txt
	for l in py r cpp rs; do morloc make -o nexus-$$l main-$$l.loc 2>> build.err; done
	for l in py r cpp rs; do \
	  echo "== $$l deep" >> obs.txt; ./nexus-$$l deep 2 2>&1 | $(STRIP) >> obs.txt; \
	  echo "== $$l caught" >> obs.txt; ./nexus-$$l caught 2 2>&1 | $(STRIP) >> obs.txt; \
	done

clean:
	rm -rf nexus-* pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
