all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus countdown 10 >> obs.txt 2>> obs.err
	./nexus countdown 1000000 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
