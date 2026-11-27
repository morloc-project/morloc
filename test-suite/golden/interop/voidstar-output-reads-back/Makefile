all:
	rm -f *.err obs.txt small.vs big.vs
	morloc make -o nexus main.loc 2> build.err
	./nexus -f voidstar mkInts 10 > small.vs 2>> obs.err
	./nexus lenInts small.vs > obs.txt 2>> obs.err
	./nexus -f voidstar mkInts 3000000 > big.vs 2>> obs.err
	./nexus lenInts big.vs >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *-build *build.tmp* small.vs big.vs
