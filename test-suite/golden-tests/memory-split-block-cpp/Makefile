all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus testBoolChain > obs.txt 2> obs.err
	./nexus testShortStrChain >> obs.txt 2>> obs.err
	./nexus testMixedChain >> obs.txt 2>> obs.err
	./nexus testMultiHop >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *err
