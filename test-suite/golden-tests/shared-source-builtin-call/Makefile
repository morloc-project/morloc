all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus t >> obs.txt 2>> obs.err
	./nexus sameBox [1,2] [1,2] >> obs.txt 2>> obs.err
	./nexus sameBox [1,2] [2,1] >> obs.txt 2>> obs.err
	./nexus roundTrip [3,4] >> obs.txt 2>> obs.err
	./nexus viaCpp [5,6,7] >> obs.txt 2>> obs.err

clean:
	rm -rf nexus __pycache__ *.err *-build *build.tmp*
