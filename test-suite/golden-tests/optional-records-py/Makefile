all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus --keep-null testMakePerson > obs.txt 2> obs.err
	./nexus --keep-null testOptionalField >> obs.txt 2>> obs.err
	./nexus --keep-null testFindPerson >> obs.txt 2>> obs.err
	./nexus --keep-null testFindPersonMissing >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
