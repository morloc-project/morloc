all:
	rm -f *.err
	: > obs.txt
	rm -f /tmp/rust-ifw-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus testIntIndex    "/tmp/rust-ifw-int.dat"    >> obs.txt 2>> obs.err
	./nexus testStrIndex    "/tmp/rust-ifw-str.dat"    >> obs.txt 2>> obs.err
	./nexus testIntSlice    "/tmp/rust-ifw-slice.dat"  >> obs.txt 2>> obs.err
	./nexus testIntTail     "/tmp/rust-ifw-tail.dat"   >> obs.txt 2>> obs.err
	./nexus testRecordField "/tmp/rust-ifw-rec.dat"    >> obs.txt 2>> obs.err
	rm -f /tmp/rust-ifw-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/rust-ifw-*.dat
	rm -rf *-build *build.tmp*
