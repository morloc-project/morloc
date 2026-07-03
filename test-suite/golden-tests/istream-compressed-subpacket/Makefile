all:
	rm -f *.err
	: > obs.txt
	rm -f /tmp/istream-compressed-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus testCompressedReadback   "/tmp/istream-compressed-a.dat" >> obs.txt 2>> obs.err
	./nexus testCompressedThreeBatch "/tmp/istream-compressed-b.dat" >> obs.txt 2>> obs.err
	rm -f /tmp/istream-compressed-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/istream-compressed-*.dat
