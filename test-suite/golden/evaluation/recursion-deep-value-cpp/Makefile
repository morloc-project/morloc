all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus chainCount 10 >> obs.txt 2>> obs.err
	./nexus chainCount 300000 >> obs.txt 2>> obs.err
	./nexus sharedCount 300000 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
