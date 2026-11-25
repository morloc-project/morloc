all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus crossRecord 3 4 >> obs.txt 2>> obs.err
	./nexus crossList 3 4 >> obs.txt 2>> obs.err
	./nexus crossTuple 3 4 >> obs.txt 2>> obs.err
	./nexus crossOpt 3 >> obs.txt 2>> obs.err
	./nexus crossListOfRecords 3 4 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
