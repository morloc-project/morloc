all:
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus goPy >> obs.txt 2> obs.err
	./nexus goR >> obs.txt 2>> obs.err
	./nexus goCpp >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
