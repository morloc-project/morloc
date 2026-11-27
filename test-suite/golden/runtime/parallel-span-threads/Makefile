all:
	rm -f obs.txt *.err calls.log
	morloc make -o nexus main.loc 2> build.err
	MORLOC_SHM_POISON=1 ./nexus tables 12 > obs.txt 2>> obs.err
	MORLOC_SHM_POISON=1 ./nexus closures 12 >> obs.txt 2>> obs.err
	MORLOC_SHM_POISON=1 ./nexus lists 12 >> obs.txt 2>> obs.err
	./nexus forks 12 >> obs.txt 2>> obs.err
	sort calls.log >> obs.txt
	rm -f calls.log

clean:
	rm -rf nexus pools __pycache__ *.err calls.log
	rm -rf *-build *build.tmp*
