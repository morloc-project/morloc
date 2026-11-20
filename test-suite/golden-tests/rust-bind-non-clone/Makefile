all:
	: > obs.txt
	rm -f *.err s.pkt
	morloc make -o nexus main.loc 2> build.err
	./nexus pickIt 3 >> obs.txt 2>> obs.err
	if ./nexus pickIt 0 >> obs.txt 2> pick.err ; then echo UNEXPECTED >> obs.txt ; else grep -c "no tallies" pick.err >> obs.txt ; fi ; cat pick.err >> obs.err
	./nexus -f packet -o s.pkt writeIt 2>> obs.err
	./nexus readIt s.pkt >> obs.txt 2>> obs.err

clean:
	rm -f *.err nexus s.pkt
	rm -rf *-build *build.tmp*
