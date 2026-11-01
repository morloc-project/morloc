all:
	: > obs.txt
	rm -f *.err ints.dat loc.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus writeInts ints.dat 2>> obs.err
	./nexus asScalar ints.dat >> obs.txt 2>> obs.err
	./nexus asList ints.dat >> obs.txt 2>> obs.err
	./nexus saveHere loc.dat > /dev/null 2>> obs.err
	./nexus asRecord loc.dat >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__ *.err ints.dat loc.dat
	rm -rf *-build *build.tmp*
