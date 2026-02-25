all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus testCoerceArg > obs.txt 2> obs.err
	./nexus testCoerceFromNull >> obs.txt 2>> obs.err
	./nexus testCoerceAddOpt >> obs.txt 2>> obs.err
	./nexus testCoerceReturn >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
