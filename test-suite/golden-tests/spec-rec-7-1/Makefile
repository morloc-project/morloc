all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus idLoc '{"pos":7}' >> obs.txt 2>> obs.err
	./nexus idEdge '{"src":1,"dst":2}' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err *-build *build.tmp*
