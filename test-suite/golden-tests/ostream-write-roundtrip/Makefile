all:
	rm -f *.err
	: > obs.txt
	rm -f /tmp/ostream-rt-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus testWriteOne          "/tmp/ostream-rt-one.dat"                            >> obs.txt 2>> obs.err
	./nexus testWriteMulti        "/tmp/ostream-rt-multi.dat"                          >> obs.txt 2>> obs.err
	./nexus testWriteIStream      "/tmp/ostream-rt-istream.dat"                        >> obs.txt 2>> obs.err
	./nexus testAppendThenLength  "/tmp/ostream-rt-append.dat"                         >> obs.txt 2>> obs.err
	./nexus testConcatTwo         "/tmp/ostream-rt-cat-a.dat" "/tmp/ostream-rt-cat-b.dat" "/tmp/ostream-rt-cat-dest.dat" >> obs.txt 2>> obs.err
	rm -f /tmp/ostream-rt-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/ostream-rt-*.dat
