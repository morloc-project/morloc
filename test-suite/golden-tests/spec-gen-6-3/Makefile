all:
	rm -f *.err obs.txt
	morloc make -o nexus-cpp main-cpp.loc 2> build-cpp.err
	./nexus-cpp fa '{"x":1}' >> obs.txt 2>> obs.err
	morloc make -o nexus-rust main-rust.loc 2> build-rust.err
	./nexus-rust fa '{"x":1}' >> obs.txt 2>> obs.err
	morloc make -o nexus-py main-py.loc 2> build-py.err
	./nexus-py fa '{"x":1}' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus-* *-build *build.tmp* *.err __pycache__
