all:
	rm -f *.err obs.txt
	morloc make -o nexus-cpp main-cpp.loc 2> build-cpp.err
	./nexus-cpp mkU 37 >> obs.txt 2>> obs.err
	./nexus-cpp readU '{"x":37,"y":{"z":38}}' >> obs.txt 2>> obs.err
	./nexus-cpp tripU '{"x":37,"y":{"z":38}}' >> obs.txt 2>> obs.err
	./nexus-cpp bumpU '{"x":37,"y":{"z":38}}' >> obs.txt 2>> obs.err
	morloc make -o nexus-rust main-rust.loc 2> build-rust.err
	./nexus-rust mkU 37 >> obs.txt 2>> obs.err
	./nexus-rust readU '{"x":37,"y":{"z":38}}' >> obs.txt 2>> obs.err
	./nexus-rust tripU '{"x":37,"y":{"z":38}}' >> obs.txt 2>> obs.err
	./nexus-rust bumpU '{"x":37,"y":{"z":38}}' >> obs.txt 2>> obs.err
	morloc make -o nexus-h2 h2-rust.loc 2> build-h2.err
	./nexus-h2 h2 '{"d":"2020-01-02"}' >> obs.txt 2>> obs.err
	./nexus-h2 showH2 '{"d":"2020-01-02"}' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus-cpp nexus-rust nexus-h2 pools __pycache__ *err *-build *build.tmp*
