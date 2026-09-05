all:
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus crossShared 5   >  obs.txt 2>  obs.err
	./nexus crossShared 21  >> obs.txt 2>> obs.err
	./nexus effShared       >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err __pycache__
	rm -rf *-build *build.tmp*
