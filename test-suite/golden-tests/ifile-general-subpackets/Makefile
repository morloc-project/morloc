all:
	rm -f *.err obs.txt /tmp/ifile-gsp-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus walkCpp           "/tmp/ifile-gsp-cpp.dat"  >  obs.txt 2>  obs.err
	./nexus walkPy            "/tmp/ifile-gsp-py.dat"   >> obs.txt 2>> obs.err
	./nexus walkCompressedCpp "/tmp/ifile-gsp-zcpp.dat" >> obs.txt 2>> obs.err
	./nexus walkCompressedPy  "/tmp/ifile-gsp-zpy.dat"  >> obs.txt 2>> obs.err
	./nexus indexThenBracket  "/tmp/ifile-gsp-idx.dat"  >> obs.txt 2>> obs.err
	./nexus sliceThenBracket  "/tmp/ifile-gsp-slc.dat"  >> obs.txt 2>> obs.err
	rm -f /tmp/ifile-gsp-*.dat

clean:
	rm -rf nexus pools __pycache__
	rm -f *.err
	rm -f /tmp/ifile-gsp-*.dat
	rm -rf *-build *build.tmp*
