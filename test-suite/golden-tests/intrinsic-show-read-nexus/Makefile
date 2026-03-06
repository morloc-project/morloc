all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus showInt > obs.txt 2> obs.err
	./nexus showReal >> obs.txt 2>> obs.err
	./nexus showBool >> obs.txt 2>> obs.err
	./nexus showStr >> obs.txt 2>> obs.err
	./nexus showList >> obs.txt 2>> obs.err
	./nexus showTuple >> obs.txt 2>> obs.err
	./nexus showRecord >> obs.txt 2>> obs.err
	./nexus showNested >> obs.txt 2>> obs.err
	./nexus showOptSome >> obs.txt 2>> obs.err
	./nexus showOptNone >> obs.txt 2>> obs.err
	./nexus showEmptyList >> obs.txt 2>> obs.err
	./nexus readInt >> obs.txt 2>> obs.err
	./nexus readReal >> obs.txt 2>> obs.err
	./nexus readBool >> obs.txt 2>> obs.err
	./nexus readStr >> obs.txt 2>> obs.err
	./nexus readList >> obs.txt 2>> obs.err
	./nexus readTuple >> obs.txt 2>> obs.err
	./nexus readRecord >> obs.txt 2>> obs.err
	./nexus readInvalid >> obs.txt 2>> obs.err
	./nexus readTypeMismatch >> obs.txt 2>> obs.err
	./nexus readEmptyStr >> obs.txt 2>> obs.err
	./nexus roundTripInt >> obs.txt 2>> obs.err
	./nexus roundTripReal >> obs.txt 2>> obs.err
	./nexus roundTripBool >> obs.txt 2>> obs.err
	./nexus roundTripStr >> obs.txt 2>> obs.err
	./nexus roundTripList >> obs.txt 2>> obs.err
	./nexus roundTripTuple >> obs.txt 2>> obs.err
	./nexus roundTripRecord >> obs.txt 2>> obs.err
	./nexus roundTripNested >> obs.txt 2>> obs.err
	./nexus showShowInt >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
