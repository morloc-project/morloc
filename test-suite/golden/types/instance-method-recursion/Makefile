all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	-./nexus listSize '[1,2,3]' > obs.txt 2> obs.err
	-./nexus nestedSize '[[1,2],[3]]' >> obs.txt 2>> obs.err
	-./nexus pairSize '[1,2]' '[[1],[2,3]]' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus __pycache__ *.err
	rm -rf *-build *build.tmp*
