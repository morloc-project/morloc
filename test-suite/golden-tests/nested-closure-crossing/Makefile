all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus pyHofRustArg 5 >> obs.txt 2>> obs.err
	./nexus rustHofPyArg 5 >> obs.txt 2>> obs.err
	./nexus captured 5 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus __pycache__ *.err
	rm -rf *-build *build.tmp*
