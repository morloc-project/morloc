all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus returned abc >> obs.txt 2>> obs.err
	./nexus inRecord abc >> obs.txt 2>> obs.err
	./nexus inList abc >> obs.txt 2>> obs.err
	./nexus viaCombinator abc >> obs.txt 2>> obs.err
	./nexus chosen true 4 >> obs.txt 2>> obs.err
	./nexus chosenThunk true 4 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus *.err
	rm -rf *-build *build.tmp*
