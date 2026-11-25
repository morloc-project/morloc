all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus forestId '{"val":1,"kids":[{"val":2,"kids":[]}]}' >> obs.txt 2>> obs.err
	./nexus bushId '{"Rose":[1,[{"Rose":[2,[]]}]]}' >> obs.txt 2>> obs.err
	./nexus forestsId '[{"val":1,"kids":[{"val":2,"kids":[]}]}]' >> obs.txt 2>> obs.err
	./nexus woodId '{"f":{"val":1,"kids":[]},"t":{"val":2,"kids":[{"val":3,"kids":[]}]}}' >> obs.txt 2>> obs.err
	./nexus nodeId '{"Node":[1,[{"Node":[2,[]]}]]}' >> obs.txt 2>> obs.err
	./nexus grove '{"Node":[1,[{"Node":[3,[]]}]]}' >> obs.txt 2>> obs.err
	./nexus nlId '[[[]]]' >> obs.txt 2>> obs.err
	./nexus rbId '{"Rose":[{"Rose":[1,[{"Rose":[2,[]]}]]},[]]}' >> obs.txt 2>> obs.err
	if ./nexus rbId '{"Rose":[{"Rose":[1,[{"Rose":[{"Rose":[2,[]]},[]]}]]},[]]}' >> obs.txt 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo REFUSED >> obs.txt; fi
	./nexus rrId '{"Rose":[{"Rose":[1,[]]},[]]}' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *err *-build *build.tmp*
