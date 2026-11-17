all:
	rm -f *.err obs.txt
	morloc make -o nexus-cpp main-cpp.loc 2> build-cpp.err
	./nexus-cpp mkU 37 >> obs.txt 2>> obs.err
	./nexus-cpp readU '{"x":37,"y":{"z":38}}' >> obs.txt 2>> obs.err
	./nexus-cpp tripU '{"x":37,"y":{"z":38}}' >> obs.txt 2>> obs.err
	./nexus-cpp bumpU '{"x":37,"y":{"z":38}}' >> obs.txt 2>> obs.err
	if morloc make -o nexus-rust main-rust.loc 2> build-rust.err; then echo UNEXPECTED >> obs.txt; elif grep -q "cannot serve one of its uses" build-rust.err; then echo REFUSED >> obs.txt; else echo UNEXPECTED >> obs.txt; fi
	if morloc make -o nexus-h2 h2-rust.loc 2> build-h2.err; then echo UNEXPECTED >> obs.txt; elif grep -q "field types differ" build-h2.err; then echo REFUSED >> obs.txt; else echo UNEXPECTED >> obs.txt; fi

clean:
	rm -rf nexus-cpp nexus-rust nexus-h2 pools __pycache__ *err *-build *build.tmp*
