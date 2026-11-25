all:
	rm -f *.err obs.txt tick.log
	morloc make -o nexus main.loc 2> build.err
	./nexus both > obs.txt 2> obs.err
	./nexus firstOnly >> obs.txt 2>> obs.err
	./nexus secondOnly >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus mixedCount >> obs.txt 2>> obs.err
	./nexus whereH >> obs.txt 2>> obs.err

clean:
	rm -rf nexus __pycache__ *.err tick.log
	rm -rf *-build *build.tmp*
