all:
	rm -f *.err obs.txt
	morloc make -o nexus-cpp main-cpp.loc 2> build-cpp.err
	./nexus-cpp sumCode 5 >> obs.txt 2>> obs.err
	./nexus-cpp sameCode 5 >> obs.txt 2>> obs.err
	morloc make -o nexus-rust main-rust.loc 2> build-rust.err
	./nexus-rust sumCode 5 >> obs.txt 2>> obs.err
	./nexus-rust sameCode 5 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus-* *-build *build.tmp* *.err
