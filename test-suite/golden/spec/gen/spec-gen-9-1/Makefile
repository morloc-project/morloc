all:
	rm -f *.err obs.txt
	morloc make -o nexus-cpp main-cpp.loc 2> build-cpp.err
	echo "--- cpp ---" >> obs.txt
	./nexus-cpp mkPair 3 >> obs.txt 2>> obs.err
	./nexus-cpp mkOuter hi >> obs.txt 2>> obs.err
	./nexus-cpp swapPair '{"fst":"a","snd":"b"}' >> obs.txt 2>> obs.err
	./nexus-cpp nested 5 >> obs.txt 2>> obs.err
	./nexus-cpp mkDates 4 >> obs.txt 2>> obs.err
	./nexus-cpp firstDate '{"fst":"7","snd":"8"}' >> obs.txt 2>> obs.err
	./nexus-cpp viaPy 1.5 >> obs.txt 2>> obs.err
	./nexus-cpp bigPair 200000 >> obs.txt 2>> obs.err
	./nexus-cpp pairTree 7 >> obs.txt 2>> obs.err
	morloc make -o nexus-rust main-rust.loc 2> build-rust.err
	echo "--- rust ---" >> obs.txt
	./nexus-rust mkPair 3 >> obs.txt 2>> obs.err
	./nexus-rust mkOuter hi >> obs.txt 2>> obs.err
	./nexus-rust swapPair '{"fst":"a","snd":"b"}' >> obs.txt 2>> obs.err
	./nexus-rust nested 5 >> obs.txt 2>> obs.err
	./nexus-rust mkDates 4 >> obs.txt 2>> obs.err
	./nexus-rust firstDate '{"fst":"7","snd":"8"}' >> obs.txt 2>> obs.err
	./nexus-rust viaPy 1.5 >> obs.txt 2>> obs.err
	./nexus-rust bigPair 200000 >> obs.txt 2>> obs.err
	./nexus-rust pairTree 7 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus-cpp nexus-rust pools __pycache__ *err *-build *build.tmp*
