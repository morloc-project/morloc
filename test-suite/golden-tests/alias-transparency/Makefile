all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus leavesOf 7 > obs.txt 2>> obs.err
	./nexus sizeOf '[[1,2],[3,4],[5,6]]' >> obs.txt 2>> obs.err
	./nexus depth 2 x >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__ *.err
	rm -rf *-build *build.tmp*
