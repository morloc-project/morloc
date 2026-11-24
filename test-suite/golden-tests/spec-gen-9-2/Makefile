all:
	rm -f *.err obs.txt
	morloc make -o nexus-cpp main-cpp.loc 2> build-cpp.err
	./nexus-cpp mkTenths 37 >> obs.txt 2>> obs.err
	./nexus-cpp sumP '{"fst":37,"snd":5}' >> obs.txt 2>> obs.err
	morloc make -o nexus-rust main-rust.loc 2> build-rust.err
	./nexus-rust mkTenths 37 >> obs.txt 2>> obs.err
	./nexus-rust sumP '{"fst":37,"snd":5}' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus-cpp nexus-rust pools __pycache__ *err *-build *build.tmp*
