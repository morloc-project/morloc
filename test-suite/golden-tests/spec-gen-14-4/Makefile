all:
	rm -f *.err obs.txt
	morloc make -o nexus-cpp main-cpp.loc 2> build-cpp.err
	echo "--- cpp ---" >> obs.txt
	./nexus-cpp showInt 3 >> obs.txt 2>> obs.err
	./nexus-cpp showDate 4 >> obs.txt 2>> obs.err
	./nexus-cpp hashInt 3 >> obs.txt 2>> obs.err
	./nexus-cpp hashDate 4 >> obs.txt 2>> obs.err
	rm -f pairs-cpp.dat
	./nexus-cpp writePairs pairs-cpp.dat >> obs.txt 2>> obs.err
	morloc make --debug -o debug-cpp main-cpp.loc 2> build-debug-cpp.err
	./debug-cpp takeDate '{"fst":"5","snd":"6"}' >> obs.txt 2>> obs.err
	morloc make -o nexus-rust main-rust.loc 2> build-rust.err
	echo "--- rust ---" >> obs.txt
	./nexus-rust showInt 3 >> obs.txt 2>> obs.err
	./nexus-rust showDate 4 >> obs.txt 2>> obs.err
	./nexus-rust hashInt 3 >> obs.txt 2>> obs.err
	./nexus-rust hashDate 4 >> obs.txt 2>> obs.err
	rm -f pairs-rust.dat
	./nexus-rust writePairs pairs-rust.dat >> obs.txt 2>> obs.err
	morloc make --debug -o debug-rust main-rust.loc 2> build-debug-rust.err
	./debug-rust takeDate '{"fst":"5","snd":"6"}' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus-cpp nexus-rust debug-cpp debug-rust pairs-*.dat pools __pycache__ *err *-build *build.tmp*
