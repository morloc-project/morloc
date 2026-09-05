all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus ack 2 2 >> obs.txt 2>> obs.err
	./nexus ack 2 3 >> obs.txt 2>> obs.err
	./nexus ack 3 3 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
