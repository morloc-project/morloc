all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus testPyToR > obs.txt 2> obs.err
	./nexus testPyToRNull >> obs.txt 2>> obs.err
	./nexus testRToPy >> obs.txt 2>> obs.err
	./nexus testRToPyNull >> obs.txt 2>> obs.err
	./nexus testChainPyR >> obs.txt 2>> obs.err
	./nexus testChainRPy >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
