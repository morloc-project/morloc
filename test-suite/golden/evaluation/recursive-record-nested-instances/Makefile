all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus trip '{"val":"a","next":null,"s":{"val":"b","next":null,"s":null,"i":null},"i":{"val":7,"next":null,"s":{"val":"c","next":null,"s":null,"i":null},"i":null}}' > obs.txt 2> obs.err

clean:
	rm -rf nexus pools *err *-build *build.tmp*
