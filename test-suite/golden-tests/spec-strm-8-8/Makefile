all:
	rm -f obs.txt *.err
	for l in py r cpp rs; do morloc make -o nexus-$$l main-$$l.loc 2>> build.err; done
	for l in py r cpp rs; do for c in seq returns raises viaValue; do echo "== $$l $$c" >> obs.txt; ./nexus-$$l $$c >> obs.txt 2>> obs.err; echo " exit $$?" >> obs.txt; done; done
	for l in py r; do echo "== $$l pool" >> obs.txt; ./nexus-$$l pool 2>> obs.err | tr -d '[]' | tr ',' '\n' | sort -n | paste -sd, >> obs.txt; done

clean:
	rm -rf nexus-* pools __pycache__ *.err
	rm -rf *-build *build.tmp*
