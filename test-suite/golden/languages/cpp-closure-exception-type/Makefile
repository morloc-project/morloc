STRIP = sed -e 's/mid=[0-9]*, //g' -e 's/(pid [0-9]*)//'

all:
	rm -f obs.txt *.err
	morloc make -o nexus main.loc 2> build.err
	for c in typed deep tried nested cstr; do echo "== $$c" >> obs.txt; ./nexus $$c 2 2>&1 | $(STRIP) >> obs.txt; done
	morloc make --debug -o nexus-debug main.loc 2>> build.err
	for c in typed deep tried; do echo "== $$c --debug" >> obs.txt; ./nexus-debug $$c 2 2>&1 | sed -n 1,2p >> obs.txt; done

clean:
	rm -rf nexus nexus-debug pools *.err .morloc-debug
	rm -rf *-build *build.tmp*
