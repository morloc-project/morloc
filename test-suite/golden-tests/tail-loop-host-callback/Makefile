all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	-./nexus run 10 >> obs.txt 2>> obs.err
	-./nexus runPartial 5 10 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err
	rm -rf *-build *build.tmp*
