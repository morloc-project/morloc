all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus testNull > obs.txt 2> obs.err
	./nexus testValue >> obs.txt 2>> obs.err
	./nexus testFromNull >> obs.txt 2>> obs.err
	./nexus testSafeHead >> obs.txt 2>> obs.err
	./nexus testSafeHeadEmpty >> obs.txt 2>> obs.err
	./nexus testOptionalAdd >> obs.txt 2>> obs.err
	./nexus testOptionalAddNull >> obs.txt 2>> obs.err
	./nexus testOptionalList >> obs.txt 2>> obs.err
	./nexus testCountNulls >> obs.txt 2>> obs.err
	./nexus testNestedNull >> obs.txt 2>> obs.err
	./nexus testNestedValue >> obs.txt 2>> obs.err
	./nexus testNestedRoundTrip >> obs.txt 2>> obs.err
	./nexus testNestedRoundTripNull >> obs.txt 2>> obs.err
	./nexus testNestedJustNull >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
