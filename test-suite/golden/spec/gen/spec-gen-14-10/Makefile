all:
	rm -f *.err obs.txt
	morloc make -o nexus-cpp main-cpp.loc 2> build-cpp.err
	./nexus-cpp nOf 4 >> obs.txt 2>> obs.err
	morloc make -o nexus-rust main-rust.loc 2> build-rust.err
	./nexus-rust nOf 5 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus-cpp nexus-rust pools __pycache__ *err *-build *build.tmp*
