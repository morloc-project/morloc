all:
	rm -f *.err obs.txt /tmp/ifile-iip-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus idxCpp /tmp/ifile-iip-idxCpp.dat >> obs.txt 2>> obs.err
	./nexus varCpp /tmp/ifile-iip-varCpp.dat >> obs.txt 2>> obs.err
	./nexus fldCpp /tmp/ifile-iip-fldCpp.dat >> obs.txt 2>> obs.err
	./nexus idxPy /tmp/ifile-iip-idxPy.dat >> obs.txt 2>> obs.err
	./nexus varPy /tmp/ifile-iip-varPy.dat >> obs.txt 2>> obs.err
	./nexus fldPy /tmp/ifile-iip-fldPy.dat >> obs.txt 2>> obs.err
	./nexus idxR /tmp/ifile-iip-idxR.dat >> obs.txt 2>> obs.err
	./nexus varR /tmp/ifile-iip-varR.dat >> obs.txt 2>> obs.err
	./nexus fldR /tmp/ifile-iip-fldR.dat >> obs.txt 2>> obs.err
	rm -f /tmp/ifile-iip-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
	rm -f /tmp/ifile-iip-*.dat
