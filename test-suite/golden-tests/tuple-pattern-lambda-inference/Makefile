all:
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus keys      >> obs.txt 2>> obs.err
	./nexus vals      >> obs.txt 2>> obs.err
	./nexus swapped   >> obs.txt 2>> obs.err
	./nexus doubled   >> obs.txt 2>> obs.err
	./nexus threeWide >> obs.txt 2>> obs.err
	./nexus nested    >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err
