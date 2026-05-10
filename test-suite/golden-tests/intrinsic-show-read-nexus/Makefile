all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus --keep-null showInt > obs.txt 2> obs.err
	./nexus --keep-null showReal >> obs.txt 2>> obs.err
	./nexus --keep-null showBool >> obs.txt 2>> obs.err
	./nexus --keep-null showStr >> obs.txt 2>> obs.err
	./nexus --keep-null showList >> obs.txt 2>> obs.err
	./nexus --keep-null showTuple >> obs.txt 2>> obs.err
	./nexus --keep-null showRecord >> obs.txt 2>> obs.err
	./nexus --keep-null showNested >> obs.txt 2>> obs.err
	./nexus --keep-null showOptSome >> obs.txt 2>> obs.err
	./nexus --keep-null showOptNone >> obs.txt 2>> obs.err
	./nexus --keep-null showEmptyList >> obs.txt 2>> obs.err
	./nexus --keep-null readInt >> obs.txt 2>> obs.err
	./nexus --keep-null readReal >> obs.txt 2>> obs.err
	./nexus --keep-null readBool >> obs.txt 2>> obs.err
	./nexus --keep-null readStr >> obs.txt 2>> obs.err
	./nexus --keep-null readList >> obs.txt 2>> obs.err
	./nexus --keep-null readTuple >> obs.txt 2>> obs.err
	./nexus --keep-null readRecord >> obs.txt 2>> obs.err
	./nexus --keep-null readInvalid >> obs.txt 2>> obs.err
	./nexus --keep-null readTypeMismatch >> obs.txt 2>> obs.err
	./nexus --keep-null readEmptyStr >> obs.txt 2>> obs.err
	./nexus --keep-null roundTripInt >> obs.txt 2>> obs.err
	./nexus --keep-null roundTripReal >> obs.txt 2>> obs.err
	./nexus --keep-null roundTripBool >> obs.txt 2>> obs.err
	./nexus --keep-null roundTripStr >> obs.txt 2>> obs.err
	./nexus --keep-null roundTripList >> obs.txt 2>> obs.err
	./nexus --keep-null roundTripTuple >> obs.txt 2>> obs.err
	./nexus --keep-null roundTripRecord >> obs.txt 2>> obs.err
	./nexus --keep-null roundTripNested >> obs.txt 2>> obs.err
	./nexus --keep-null showShowInt >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
