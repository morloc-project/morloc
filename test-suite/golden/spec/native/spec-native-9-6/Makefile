all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus literal > obs.txt 2> obs.err
	./nexus literalObject >> obs.txt 2>> obs.err
	./nexus received 3 >> obs.txt 2>> obs.err
	./nexus received 1000000 >> obs.txt 2>> obs.err
	./nexus receivedObject 3 >> obs.txt 2>> obs.err
	./nexus receivedObject 100000 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *err
	rm -rf *-build *build.tmp*
