all:
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus dash >> obs.txt 2> obs.err
	./nexus under >> obs.txt 2>> obs.err
	./nexus upper >> obs.txt 2>> obs.err
	./nexus dotted >> obs.txt 2>> obs.err
	./nexus relative >> obs.txt 2>> obs.err
	./nexus countA >> obs.txt 2>> obs.err
	./nexus countB >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ lib.v2/__pycache__ my-pkg/__pycache__ *.err
	rm -rf *-build *build.tmp*
