all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus good 2 >> obs.txt 2>> obs.err
	./nexus bad 2 >> obs.txt 2>> obs.err && echo UNEXPECTED >> obs.txt || echo OK >> obs.txt
	./nexus good 3 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
