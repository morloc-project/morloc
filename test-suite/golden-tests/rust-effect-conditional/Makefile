all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus pick true        >> obs.txt 2>> obs.err
	./nexus pick false       >> obs.txt 2>> obs.err
	./nexus pickUnit true    >> obs.txt 2>> obs.err
	./nexus pickDo true      >> obs.txt 2>> obs.err
	./nexus pickNested true false >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err
	rm -rf *-build *build.tmp*
