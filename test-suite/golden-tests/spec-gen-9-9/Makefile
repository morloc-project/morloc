all:
	rm -f *.err obs.txt
	morloc make -o nexus-cpp main-cpp.loc 2> build-cpp.err
	./nexus-cpp trip '{"xs":[1,2],"tag":3}' >> obs.txt 2>> obs.err
	./nexus-cpp tripAny '{"xs":[1,2],"tag":3}' >> obs.txt 2>> obs.err
	./nexus-cpp tripArr '{"xs":[1,2],"tag":3}' >> obs.txt 2>> obs.err
	morloc make -o nexus-rust main-rust.loc 2> build-rust.err
	./nexus-rust trip '{"xs":[1,2],"tag":3}' >> obs.txt 2>> obs.err
	./nexus-rust tripAny '{"xs":[1,2],"tag":3}' >> obs.txt 2>> obs.err
	./nexus-rust tripArr '{"xs":[1,2],"tag":3}' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus-cpp nexus-rust pools __pycache__ *err *-build *build.tmp*
