all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus testShow > obs.txt 2> obs.err
	./nexus testRead >> obs.txt 2>> obs.err
	./nexus testRoundTrip >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
