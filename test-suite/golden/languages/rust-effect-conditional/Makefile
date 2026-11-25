all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus pick true        >> obs.txt 2>> obs.err
	./nexus pick false       >> obs.txt 2>> obs.err
	./nexus pickUnit true    >> obs.txt 2>> obs.err
	./nexus pickDo true      >> obs.txt 2>> obs.err
	./nexus pickNested true false >> obs.txt 2>> obs.err
	rm -f cnt.dat
	./nexus forcedTwice $$PWD/cnt.dat true >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err cnt.dat
	rm -rf *-build *build.tmp*
