all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./run.sh >> obs.txt 2>> obs.err
	./nexus chatty 70000 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err started.txt
	rm -rf *-build *build.tmp*
