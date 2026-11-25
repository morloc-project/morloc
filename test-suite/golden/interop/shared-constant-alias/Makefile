all:
	rm -f *.err obs.txt tick.log
	morloc make -o nexus main.loc 2> build.err
	morloc make -o imported imported.loc 2>> build.err
	morloc make -o cpp main-cpp.loc 2>> build.err
	./nexus nullaryCtor 3 > obs.txt 2> obs.err
	./nexus aliasLiteral 4 >> obs.txt 2>> obs.err
	./nexus aliasList 4 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus aliasCount 1 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus aliasOfAliasCount 1 >> obs.txt 2>> obs.err
	./nexus aliasCommand >> obs.txt 2>> obs.err
	./nexus aliasOfAliasCommand >> obs.txt 2>> obs.err
	./imported aliasImported >> obs.txt 2>> obs.err
	./cpp nullaryCtor 3 >> obs.txt 2>> obs.err
	./cpp aliasLiteral 4 >> obs.txt 2>> obs.err
	./cpp aliasList 4 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus imported cpp __pycache__ *.err tick.log
	rm -rf *-build *build.tmp*
