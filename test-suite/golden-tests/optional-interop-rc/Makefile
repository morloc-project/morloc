all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus testRToCpp > obs.txt 2> obs.err
	./nexus testRToCppNull >> obs.txt 2>> obs.err
	./nexus testCppToR >> obs.txt 2>> obs.err
	./nexus testCppToRNull >> obs.txt 2>> obs.err
	./nexus testChainRCpp >> obs.txt 2>> obs.err
	./nexus testChainCppR >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
