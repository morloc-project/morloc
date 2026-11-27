all:
	rm -f *.err obs.txt /tmp/ifile-sip-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus realsCpp 0 "/tmp/ifile-sip-realsCpp0.dat" > obs.txt 2> obs.err
	./nexus realsRust 0 "/tmp/ifile-sip-realsRust0.dat" >> obs.txt 2>> obs.err
	./nexus realsPy 0 "/tmp/ifile-sip-realsPy0.dat" >> obs.txt 2>> obs.err
	./nexus realsCpp 3 "/tmp/ifile-sip-realsCpp3.dat" >> obs.txt 2>> obs.err
	./nexus realsRust 3 "/tmp/ifile-sip-realsRust3.dat" >> obs.txt 2>> obs.err
	./nexus realsPy 3 "/tmp/ifile-sip-realsPy3.dat" >> obs.txt 2>> obs.err
	./nexus strsCpp 0 "/tmp/ifile-sip-strsCpp0.dat" >> obs.txt 2>> obs.err
	./nexus strsRust 0 "/tmp/ifile-sip-strsRust0.dat" >> obs.txt 2>> obs.err
	./nexus strsPy 0 "/tmp/ifile-sip-strsPy0.dat" >> obs.txt 2>> obs.err
	./nexus strsCpp 3 "/tmp/ifile-sip-strsCpp3.dat" >> obs.txt 2>> obs.err
	./nexus strsRust 3 "/tmp/ifile-sip-strsRust3.dat" >> obs.txt 2>> obs.err
	./nexus strsPy 3 "/tmp/ifile-sip-strsPy3.dat" >> obs.txt 2>> obs.err
	./nexus pairsCpp 0 "/tmp/ifile-sip-pairsCpp0.dat" >> obs.txt 2>> obs.err
	./nexus pairsRust 0 "/tmp/ifile-sip-pairsRust0.dat" >> obs.txt 2>> obs.err
	./nexus pairsPy 0 "/tmp/ifile-sip-pairsPy0.dat" >> obs.txt 2>> obs.err
	./nexus pairsCpp 3 "/tmp/ifile-sip-pairsCpp3.dat" >> obs.txt 2>> obs.err
	./nexus pairsRust 3 "/tmp/ifile-sip-pairsRust3.dat" >> obs.txt 2>> obs.err
	./nexus pairsPy 3 "/tmp/ifile-sip-pairsPy3.dat" >> obs.txt 2>> obs.err
	rm -f /tmp/ifile-sip-*.dat

clean:
	rm -rf nexus pools __pycache__
	rm -f *.err
	rm -f /tmp/ifile-sip-*.dat
	rm -rf *-build *build.tmp*
