all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus viaSource 4 > obs.txt 2> obs.err
	./nexus viaRead 4 >> obs.txt 2>> obs.err
	./nexus viaMade 4 >> obs.txt 2>> obs.err
	./nexus viaChain 4 >> obs.txt 2>> obs.err
	./nexus viaPy 4 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err *-build *build.tmp*
