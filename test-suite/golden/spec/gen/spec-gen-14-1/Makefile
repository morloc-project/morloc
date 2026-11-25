all:
	rm -f *.err obs.txt
	morloc make -o nexus-cpp main-cpp.loc 2> build-cpp.err
	echo "--- cpp ---" >> obs.txt
	./nexus-cpp roundStamp 3 >> obs.txt 2>> obs.err
	./nexus-cpp roundBox 4 >> obs.txt 2>> obs.err
	./nexus-cpp countUnpacks 5 >> obs.txt 2>> obs.err
	./nexus-cpp roundBoxes 6 >> obs.txt 2>> obs.err
	./nexus-cpp roundCrate 7 >> obs.txt 2>> obs.err
	if ./nexus-cpp failing 1 >> obs.txt 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo FAILED >> obs.txt; fi
	morloc make -o nexus-rust main-rust.loc 2> build-rust.err
	echo "--- rust ---" >> obs.txt
	./nexus-rust roundStamp 3 >> obs.txt 2>> obs.err
	./nexus-rust roundBox 4 >> obs.txt 2>> obs.err
	./nexus-rust countUnpacks 5 >> obs.txt 2>> obs.err
	./nexus-rust roundBoxes 6 >> obs.txt 2>> obs.err
	./nexus-rust roundCrate 7 >> obs.txt 2>> obs.err
	if ./nexus-rust failing 1 >> obs.txt 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo FAILED >> obs.txt; fi

clean:
	rm -rf nexus-cpp nexus-rust pools __pycache__ *err *-build *build.tmp*
