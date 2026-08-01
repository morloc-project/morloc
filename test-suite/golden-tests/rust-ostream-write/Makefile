all:
	rm -f *.err
	: > obs.txt
	rm -f /tmp/rust-ostream-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus testWriteOne          "/tmp/rust-ostream-one.dat"     >> obs.txt 2>> obs.err
	./nexus testWriteMulti        "/tmp/rust-ostream-multi.dat"   >> obs.txt 2>> obs.err
	./nexus testWriteIStream      "/tmp/rust-ostream-istream.dat" >> obs.txt 2>> obs.err
	./nexus testAppendThenLength  "/tmp/rust-ostream-append.dat"  >> obs.txt 2>> obs.err
	./nexus testConcatTwo         "/tmp/rust-ostream-cat-a.dat" "/tmp/rust-ostream-cat-b.dat" "/tmp/rust-ostream-cat-dest.dat" >> obs.txt 2>> obs.err
	./nexus testStreamLayoutChunks "/tmp/rust-ostream-layout.dat"  >> obs.txt 2>> obs.err
	rm -f /tmp/rust-ostream-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/rust-ostream-*.dat
