all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus testMakePair > obs.txt 2> obs.err
	./nexus testGetPairFlag >> obs.txt 2>> obs.err
	./nexus testGetPairValue >> obs.txt 2>> obs.err
	./nexus testPairRoundTrip >> obs.txt 2>> obs.err
	./nexus testMakeTriple >> obs.txt 2>> obs.err
	./nexus testGetTripleCount >> obs.txt 2>> obs.err
	./nexus testGetTripleValue >> obs.txt 2>> obs.err
	./nexus testTripleRoundTrip >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
