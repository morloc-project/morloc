all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus direct '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus viaDo '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus viaLoop '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus effectfulBody '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus effectfulArgument '[1,2,3]' >> obs.txt 2>> obs.err
	morloc make -o dynamic dynamic.loc 2> dynamic.err
	./dynamic pick true '[1,2,3]' >> obs.txt 2>> obs.err
	./dynamic pick false '[1,2,3]' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus dynamic pools __pycache__ *.err
	rm -rf *-build *build.tmp*
