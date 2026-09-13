all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus bound 41 >> obs.txt 2>> obs.err
	./nexus tail 7 >> obs.txt 2>> obs.err
	./nexus held 21 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus __pycache__ *.err
	rm -rf *-build *build.tmp*
