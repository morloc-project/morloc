all:
	rm -f *.err obs.txt
	morloc make -o nexus-cpp main-cpp.loc 2> build-cpp.err
	./nexus-cpp idShape '{"Circle":["2020-01-02"]}' >> obs.txt 2>> obs.err
	./nexus-cpp idShape '{"Box":[4]}' >> obs.txt 2>> obs.err
	./nexus-cpp idOpt '{"Some":["2021-03-04"]}' >> obs.txt 2>> obs.err
	./nexus-cpp idOpt '"None"' >> obs.txt 2>> obs.err
	./nexus-cpp idOptStr '{"Some":["x"]}' >> obs.txt 2>> obs.err
	./nexus-cpp showShape '{"Circle":["2020-01-02"]}' >> obs.txt 2>> obs.err
	./nexus-cpp idDot '{"Dot":[{"fst":12,"snd":5}]}' >> obs.txt 2>> obs.err
	morloc make -o nexus-rust main-rust.loc 2> build-rust.err
	./nexus-rust idShape '{"Circle":["2020-01-02"]}' >> obs.txt 2>> obs.err
	./nexus-rust idShape '{"Box":[4]}' >> obs.txt 2>> obs.err
	./nexus-rust idOpt '{"Some":["2021-03-04"]}' >> obs.txt 2>> obs.err
	./nexus-rust idOpt '"None"' >> obs.txt 2>> obs.err
	./nexus-rust idOptStr '{"Some":["x"]}' >> obs.txt 2>> obs.err
	./nexus-rust showShape '{"Circle":["2020-01-02"]}' >> obs.txt 2>> obs.err
	./nexus-rust idDot '{"Dot":[{"fst":12,"snd":5}]}' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus-cpp nexus-rust pools __pycache__ *err *-build *build.tmp*
