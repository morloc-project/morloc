all:
	rm -f *.err obs.txt ints.stream
	morloc make -o nexus main.loc 2> build.err
	./nexus writeInts ints.stream 2> obs.err
	./nexus loadReals ints.stream >> obs.txt 2>> obs.err
	./nexus loadInts ints.stream >> obs.txt 2>> obs.err
	rm -f ints.stream

clean:
	rm -rf nexus pools __pycache__ *.err ints.stream
	rm -rf *-build *build.tmp*
