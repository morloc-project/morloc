all:
	rm -f *.err obs.txt
	rm -f /tmp/ifile-pat-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus testIntIndex          "/tmp/ifile-pat-int-index.dat"      >> obs.txt 2>> obs.err
	./nexus testStrIndex          "/tmp/ifile-pat-str-index.dat"      >> obs.txt 2>> obs.err
	./nexus testIntSlice          "/tmp/ifile-pat-int-slice.dat"      >> obs.txt 2>> obs.err
	./nexus testTupleSliceField   "/tmp/ifile-pat-tup-sf.dat"         >> obs.txt 2>> obs.err
	./nexus testTupleIndexField   "/tmp/ifile-pat-tup-if.dat"         >> obs.txt 2>> obs.err
	./nexus testTupleStructField  "/tmp/ifile-pat-tup-sf2.dat"        >> obs.txt 2>> obs.err
	./nexus testTupleStructIndex  "/tmp/ifile-pat-tup-si.dat"         >> obs.txt 2>> obs.err
	./nexus testTupleStructSlice  "/tmp/ifile-pat-tup-ss.dat"         >> obs.txt 2>> obs.err
	./nexus testTupleGroup        "/tmp/ifile-pat-tup-grp.dat"        >> obs.txt 2>> obs.err
	./nexus testNestedTupleGroup  "/tmp/ifile-pat-tup-ngrp.dat"       >> obs.txt 2>> obs.err
	rm -f /tmp/ifile-pat-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/ifile-pat-*.dat
