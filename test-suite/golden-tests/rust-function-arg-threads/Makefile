all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus direct '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus viaDo '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus viaLoop '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus effectfulBody '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus effectfulArgument '[1,2,3]' >> obs.txt 2>> obs.err
	if morloc make -o dynamic dynamic.loc > /dev/null 2> dynamic.err; then echo '"UNEXPECTED-ACCEPTED"' >> obs.txt; else echo '"rejected"' >> obs.txt; fi

clean:
	rm -rf nexus dynamic pools __pycache__ *.err
	rm -rf *-build *build.tmp*
