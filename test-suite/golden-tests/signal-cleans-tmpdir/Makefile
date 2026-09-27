all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./run.sh INT >> obs.txt 2>> obs.err
	./run.sh TERM >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err started.txt
	rm -rf *-build *build.tmp*
