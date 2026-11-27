all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus count '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus width '"abcd"' >> obs.txt 2>> obs.err
	./nexus total '[1.5,2.5]' >> obs.txt 2>> obs.err
	./nexus count 'nope' >> obs.txt 2>> obs.err || echo "count rejected" >> obs.txt

clean:
	rm -rf nexus pools *.err
	rm -rf *-build *build.tmp*
