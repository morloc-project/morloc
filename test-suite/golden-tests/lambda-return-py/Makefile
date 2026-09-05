all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus direct 5 >> obs.txt 2>> obs.err
	./nexus bound 5  >> obs.txt 2>> obs.err
	./nexus viaHof 5 >> obs.txt 2>> obs.err
	./nexus deep 5   >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
