all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus readCpp abc > obs.txt 2> obs.err
	./nexus readPy abc >> obs.txt 2>> obs.err
	./nexus readR abc >> obs.txt 2>> obs.err
	./nexus readRust abc >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
	rm -rf *-build *build.tmp*
