all:
	rm -f *.err obs.txt
	for l in rs cpp py; do morloc make -o nexus-$$l main-$$l.loc 2>> build.err; done
	./nexus-rs unpacked 3 >> obs.txt 2>> obs.err
	./nexus-rs packed 3 >> obs.txt 2>> obs.err
	for l in cpp py; do ./nexus-$$l unpacked '[[1,2],[3]]' >> obs.txt 2>> obs.err; ./nexus-$$l packed '[[1,2],[3]]' >> obs.txt 2>> obs.err; ./nexus-$$l nested '[[[1],[2,3]],[[4]]]' >> obs.txt 2>> obs.err; done

clean:
	rm -rf nexus-* pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
