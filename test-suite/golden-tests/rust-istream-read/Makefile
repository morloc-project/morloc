all:
	rm -f *.err
	: > obs.txt
	rm -f /tmp/rust-istream-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus testNextOnce        "/tmp/rust-istream-next.dat"   >> obs.txt 2>> obs.err
	./nexus testNextEofTwice    "/tmp/rust-istream-eof.dat"    >> obs.txt 2>> obs.err
	./nexus testStreamFromIFile "/tmp/rust-istream-stream.dat" >> obs.txt 2>> obs.err
	./nexus testIntList         "/tmp/rust-istream-int.dat"    >> obs.txt 2>> obs.err
	./nexus testLength          "/tmp/rust-istream-len.dat"    >> obs.txt 2>> obs.err
	./nexus testFschema         "/tmp/rust-istream-schema.dat" >> obs.txt 2>> obs.err
	rm -f /tmp/rust-istream-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/rust-istream-*.dat
