all:
	rm -f *.err obs.txt
	morloc make -o nexus-cpp main-cpp.loc 2> build-cpp.err
	./nexus-cpp idB '{"n":1,"s":"x"}' >> obs.txt 2>> obs.err
	./nexus-cpp idS '{"n":2,"s":"y"}' >> obs.txt 2>> obs.err
	morloc make -o nexus-rust main-rust.loc 2> build-rust.err
	./nexus-rust idB '{"n":1,"s":"x"}' >> obs.txt 2>> obs.err
	./nexus-rust idS '{"n":2,"s":"y"}' >> obs.txt 2>> obs.err
	morloc make -o nexus-py main-py.loc 2> build-py.err
	./nexus-py idB '{"n":1,"s":"x"}' >> obs.txt 2>> obs.err
	./nexus-py idS '{"n":2,"s":"y"}' >> obs.txt 2>> obs.err
	morloc make -o nexus-r main-r.loc 2> build-r.err
	./nexus-r idB '{"n":1,"s":"x"}' >> obs.txt 2>> obs.err
	./nexus-r idS '{"n":2,"s":"y"}' >> obs.txt 2>> obs.err
	morloc make -o nexus-tag main-tag.loc 2> build-tag.err
	./nexus-tag g 7 >> obs.txt 2>> obs.err
	morloc make -o nexus-dcpp data-cpp.loc 2> build-dcpp.err
	./nexus-dcpp idW '{"W":[{"Tagged":[3]}]}' >> obs.txt 2>> obs.err
	./nexus-dcpp idR '{"t":"Untagged","n":2}' >> obs.txt 2>> obs.err
	./nexus-dcpp idV '{"V":[{"t":{"Tagged":[4]},"n":5}]}' >> obs.txt 2>> obs.err
	./nexus-dcpp idX '"NoX"' >> obs.txt 2>> obs.err
	morloc make -o nexus-drust data-rust.loc 2> build-drust.err
	./nexus-drust idW '{"W":[{"Tagged":[3]}]}' >> obs.txt 2>> obs.err
	./nexus-drust idR '{"t":"Untagged","n":2}' >> obs.txt 2>> obs.err
	./nexus-drust idV '{"V":[{"t":{"Tagged":[4]},"n":5}]}' >> obs.txt 2>> obs.err
	./nexus-drust idX '"NoX"' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus-cpp nexus-rust nexus-py nexus-r nexus-tag nexus-dcpp nexus-drust pools __pycache__ *err *-build *build.tmp*
