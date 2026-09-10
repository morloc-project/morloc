all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus vec >> obs.txt 2>> obs.err
	./nexus mat >> obs.txt 2>> obs.err
	./nexus total >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err
	rm -rf *-build *build.tmp*
