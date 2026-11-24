all:
	rm -f *.err obs.txt
	morloc make -o nexus-cpp main-cpp.loc 2> build-cpp.err
	./nexus-cpp altCount '"a"' >> obs.txt 2>> obs.err
	./nexus-cpp roundTrip '{"label":"r","kids":[{"label":"k","kids":[],"alts":[]}],"alts":[{"Pick":[{"label":"p","kids":[],"alts":[]}]},"Skip"]}' >> obs.txt 2>> obs.err
	./nexus-cpp abTrip '{"d":{"Node":[{"xs":[{"d":"Leaf","tag":"x"},{"d":{"Node":[{"xs":[]}]},"tag":"z"}]}]},"tag":"y"}' >> obs.txt 2>> obs.err
	morloc make -o nexus-rust main-rust.loc 2> build-rust.err
	./nexus-rust altCount '"a"' >> obs.txt 2>> obs.err
	./nexus-rust roundTrip '{"label":"r","kids":[{"label":"k","kids":[],"alts":[]}],"alts":[{"Pick":[{"label":"p","kids":[],"alts":[]}]},"Skip"]}' >> obs.txt 2>> obs.err
	./nexus-rust abTrip '{"d":{"Node":[{"xs":[{"d":"Leaf","tag":"x"},{"d":{"Node":[{"xs":[]}]},"tag":"z"}]}]},"tag":"y"}' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus-cpp nexus-rust pools *err *-build *build.tmp*
