all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus twoFunctions '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus throughWrapper '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus closedLambda '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus swapped '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus partial '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus captured 10 '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus mutual 12 >> obs.txt 2>> obs.err
	./nexus aliased '[1,2,3]' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
