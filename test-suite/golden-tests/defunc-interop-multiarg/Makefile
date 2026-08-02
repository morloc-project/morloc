all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus crossTwoArg 5 >> obs.txt 2>> obs.err
	./nexus crossMultiVar 3 4 >> obs.txt 2>> obs.err
	./nexus crossList 10 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
