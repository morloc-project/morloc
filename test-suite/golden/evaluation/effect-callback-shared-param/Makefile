all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus viaPy 3 4 >> obs.txt 2>> obs.err
	./nexus viaRust 3 4 >> obs.txt 2>> obs.err
	./nexus listPy 3 4 >> obs.txt 2>> obs.err
	./nexus listRust 3 4 >> obs.txt 2>> obs.err
	./nexus sharedLet 3 4 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus __pycache__ *.err
	rm -rf *-build *build.tmp*
