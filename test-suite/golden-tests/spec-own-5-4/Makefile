all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus tupleSlot 100 >> obs.txt 2>> obs.err
	./nexus recordSlot 100 >> obs.txt 2>> obs.err
	./nexus letAlias 100 >> obs.txt 2>> obs.err
	./nexus lentParam 100 >> obs.txt 2>> obs.err
	./nexus readBefore 100 >> obs.txt 2>> obs.err
	./nexus readAfter 100 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__
	rm -f *.err
	rm -rf *-build *build.tmp*
