CMDS = pyKind rKind pyRoundTrip rRoundTrip crossKinds

all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	for c in $(CMDS); do echo "--- $$c ---" >> obs.txt; ./nexus $$c 1 >> obs.txt 2>> obs.err; done

clean:
	rm -rf nexus pools *.err __pycache__
	rm -rf *-build *build.tmp*
