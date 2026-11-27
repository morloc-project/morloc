all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus math f 5 >> obs.txt 2>> obs.err
	./nexus text echo hi >> obs.txt 2>> obs.err
	morloc make -o across across.loc 2>&1 | grep -c 'Two commands are named' >> obs.txt
	morloc make -o within within.loc 2>&1 | grep -c 'Two commands are named' >> obs.txt
	morloc make -o samegroup samegroup.loc 2>&1 | grep -c 'Two commands are named' >> obs.txt
	morloc make -o mixed mixed.loc 2>&1 | grep -c 'Two commands are named' >> obs.txt
	morloc make -o group group.loc 2>&1 | grep -c 'name of a command group' >> obs.txt
	morloc make -o twice twice.loc 2>&1 | grep -c 'exported twice' >> obs.txt

clean:
	rm -f *.err nexus across within samegroup mixed group twice
	rm -rf *-build *build.tmp*
