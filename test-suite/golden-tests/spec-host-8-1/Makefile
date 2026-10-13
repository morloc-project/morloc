all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus adder 3 4 >> obs.txt 2>> obs.err
	./nexus viaOps 3 4 >> obs.txt 2>> obs.err
	if morloc make -o unshareable unshareable.loc > /dev/null 2> unshareable.err; then echo '"UNEXPECTED-ACCEPTED"' >> obs.txt; else grep -c 'may be called from any thread' unshareable.err >> obs.txt; fi

clean:
	rm -rf nexus unshareable pools __pycache__ *.err
	rm -rf *-build *build.tmp*
