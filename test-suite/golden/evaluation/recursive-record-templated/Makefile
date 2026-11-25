all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus lenL 4 > obs.txt 2> obs.err
	./nexus sumL 4 >> obs.txt 2>> obs.err
	./nexus listOut 3 >> obs.txt 2>> obs.err
	./nexus roundTrip '{"val":5,"next":{"val":6,"next":null}}' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools
	rm -rf *-build *build.tmp*
