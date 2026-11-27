all:
	rm -f *.err obs.txt *.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus strs s.dat >> obs.txt 2>> obs.err
	./nexus bools b.dat >> obs.txt 2>> obs.err
	./nexus flags f.dat >> obs.txt 2>> obs.err
	./nexus flagsBack f.dat >> obs.txt 2>> obs.err
	./nexus reals r.dat >> obs.txt 2>> obs.err
	./nexus note 7 >> obs.txt 2>> obs.err
	./nexus oneNote n.dat >> obs.txt 2>> obs.err
	./nexus notesBack n.dat >> obs.txt 2>> obs.err
	./nexus missing false >> obs.txt 2>> obs.err
	./nexus missing true >> obs.txt 2>> obs.err && echo UNEXPECTED_MISSING_OK >> obs.txt || echo missing-refused >> obs.txt

clean:
	rm -rf nexus pools __pycache__ *.err *.dat
	rm -rf *-build *build.tmp*
