all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	timeout 60s ./nexus deepLen 1000000 > obs.txt 2> obs.err
	./nexus oneCell 5 >> obs.txt 2>> obs.err
	./nexus firstOf a >> obs.txt 2>> obs.err
	./nexus secondOf a >> obs.txt 2>> obs.err
	./nexus bothShared a >> obs.txt 2>> obs.err
	./nexus roundTerm 2.5 >> obs.txt 2>> obs.err
	./nexus viaPy 2.5 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *err *-build *build.tmp*
