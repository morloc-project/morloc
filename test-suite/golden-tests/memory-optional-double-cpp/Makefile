all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus testOptRealValue > obs.txt 2> obs.err
	./nexus testOptRealNull >> obs.txt 2>> obs.err
	./nexus testOptRealRoundTrip >> obs.txt 2>> obs.err
	./nexus testOptIntValue >> obs.txt 2>> obs.err
	./nexus testOptIntNull >> obs.txt 2>> obs.err
	./nexus testOptIntRoundTrip >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
