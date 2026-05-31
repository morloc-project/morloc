all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus cppCase > obs.txt 2> obs.err
	./nexus pyCase >> obs.txt 2>> obs.err
	./nexus rCase >> obs.txt 2>> obs.err
	./nexus cppNullCase >> obs.txt 2>> obs.err
	./nexus pyNullCase >> obs.txt 2>> obs.err
	./nexus rNullCase >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
