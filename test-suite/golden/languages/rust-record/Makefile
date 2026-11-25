all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus makePair 3 >> obs.txt 2> obs.err
	./nexus buildLL 5 >> obs.txt 2>> obs.err
	./nexus sumLL '{"head":5,"tail":{"head":6,"tail":{"head":7,"tail":null}}}' >> obs.txt 2>> obs.err
	./nexus sumTree '{"value":1,"children":[{"value":2,"children":[]},{"value":3,"children":[{"value":4,"children":[]}]}]}' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
