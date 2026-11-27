all:
	rm -f *.err
	: > obs.txt
	rm -f /tmp/collect-in-branch-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus viaGuard true  >> obs.txt 2>> obs.err
	./nexus viaGuard false >> obs.txt 2>> obs.err
	./nexus viaMatch /tmp/collect-in-branch-missing.dat >> obs.txt 2>> obs.err
	./nexus closeArm true /tmp/collect-in-branch-c.dat >> obs.txt 2>> obs.err
	./nexus viaMatch /tmp/collect-in-branch-c.dat >> obs.txt 2>> obs.err
	rm -f /tmp/collect-in-branch-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/collect-in-branch-*.dat
	rm -rf *-build *build.tmp*
