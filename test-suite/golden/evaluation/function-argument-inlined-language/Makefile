all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus viaHelper '[1,2,3]' > obs.txt 2> obs.err
	./nexus letBetween '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus letAlias '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus pick true '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus pick false '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus pickAtCall true '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus pickAtCall false '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus pickLetAtCall true '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus pickLetAtCall false '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus direct '[1,2,3]' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
