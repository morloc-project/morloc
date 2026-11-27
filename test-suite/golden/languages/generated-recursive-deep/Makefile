all:
	rm -f *.err obs.txt
	morloc make -o nexus-cpp main-cpp.loc 2> build-cpp.err
	timeout 60s ./nexus-cpp deep 1000000 > obs.txt 2> obs.err
	morloc make -o nexus-rust main-rust.loc 2> build-rust.err
	timeout 60s ./nexus-rust deep 1000000 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus-cpp nexus-rust pools *err *-build *build.tmp*
