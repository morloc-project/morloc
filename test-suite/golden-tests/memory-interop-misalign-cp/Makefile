all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus testOptRealCppToPy > obs.txt 2> obs.err
	./nexus testOptRealPyToCpp >> obs.txt 2>> obs.err
	./nexus testRecordCppToPy >> obs.txt 2>> obs.err
	./nexus testRecordPyToCpp >> obs.txt 2>> obs.err
	./nexus testOptRealChain >> obs.txt 2>> obs.err
	./nexus testRecordChain >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *err
