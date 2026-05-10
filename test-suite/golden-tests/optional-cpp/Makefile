all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus --keep-null testNull > obs.txt 2> obs.err
	./nexus --keep-null testValue >> obs.txt 2>> obs.err
	./nexus --keep-null testFromNull >> obs.txt 2>> obs.err
	./nexus --keep-null testSafeHead >> obs.txt 2>> obs.err
	./nexus --keep-null testSafeHeadEmpty >> obs.txt 2>> obs.err
	./nexus --keep-null testOptionalAdd >> obs.txt 2>> obs.err
	./nexus --keep-null testOptionalAddNull >> obs.txt 2>> obs.err
	./nexus --keep-null testOptionalList >> obs.txt 2>> obs.err
	./nexus --keep-null testCountNulls >> obs.txt 2>> obs.err
	./nexus --keep-null testNestedNull >> obs.txt 2>> obs.err
	./nexus --keep-null testNestedValue >> obs.txt 2>> obs.err
	./nexus --keep-null testNestedRoundTrip >> obs.txt 2>> obs.err
	./nexus --keep-null testNestedRoundTripNull >> obs.txt 2>> obs.err
	./nexus --keep-null testNestedJustNull >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
