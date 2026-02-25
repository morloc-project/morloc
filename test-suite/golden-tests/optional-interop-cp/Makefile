all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus testCppToPy > obs.txt 2> obs.err
	./nexus testCppToPyNull >> obs.txt 2>> obs.err
	./nexus testPyToCpp >> obs.txt 2>> obs.err
	./nexus testPyToCppNull >> obs.txt 2>> obs.err
	./nexus testChainCppPy >> obs.txt 2>> obs.err
	./nexus testChainPyCpp >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
