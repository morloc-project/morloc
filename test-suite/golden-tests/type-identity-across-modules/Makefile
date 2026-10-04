all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus total 3 >> obs.txt 2>> obs.err
	./nexus viaSink 3 >> obs.txt 2>> obs.err
	./nexus field 3 >> obs.txt 2>> obs.err
	./nexus own hi >> obs.txt 2>> obs.err
	./nexus both 3 hi >> obs.txt 2>> obs.err
	./nexus colors 3 >> obs.txt 2>> obs.err
	./nexus shapes 3 2.5 >> obs.txt 2>> obs.err
	./nexus trees 3 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
