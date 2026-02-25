all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus testMakePerson > obs.txt 2> obs.err
	./nexus testOptionalField >> obs.txt 2>> obs.err
	./nexus testFindPerson >> obs.txt 2>> obs.err
	./nexus testFindPersonMissing >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
