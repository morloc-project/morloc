all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus closeHere 0 >> obs.txt 2>> obs.err
	./nexus closeThere 0 >> obs.txt 2>> obs.err
	./nexus closeUnregistered 0 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err *-build
