all:
	rm -f obs.txt *.err
	morloc make -o nexus main.loc 2> build.err
	for k in 1 2 3; do ./nexus seq 26 $$k --s >> obs.txt 2>> obs.err; echo >> obs.txt; done

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
