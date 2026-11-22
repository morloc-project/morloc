CASES = "ifPart 7 1" "freshArm 7 1" "matchPart 7" "listPart 7" "readMoves 7" "moveReads 7" "moveReadsMk 7" "twoSources 7 1" "twoSources 7 0" "twoMoves 7 0" "twoMoves 7 1" "loopRead 7" "ladderPart 7 1" "ladderPart 7 2" "ladderPart 7 0" "wholeLive 7 1" "wholeLive 7 0" "wholeDead 7 1"

all:
	rm -f *.err obs.txt
	morloc make -o nexus-cpp main-cpp.loc 2>> build.err
	CXX="$${CXX:-g++} -fsanitize=address,undefined -fno-sanitize-recover=all" morloc make -o asan-cpp main-cpp.loc 2>> build.err
	MORLOC_LANG_PARAMS=rust:lto=off morloc make -o nexus-rs main-rs.loc 2>> build.err
	for c in $(CASES); do echo "cpp $$c $$(./nexus-cpp $$c 2>> obs.err || echo failed)" >> obs.txt; done
	for c in $(CASES); do echo "rs $$c $$(./nexus-rs $$c 2>> obs.err || echo failed)" >> obs.txt; done
	for c in $(CASES); do echo "asan $$c $$(ASAN_OPTIONS=detect_leaks=0 ./asan-cpp $$c 2>> obs.err || echo failed)" >> obs.txt; done

clean:
	rm -rf nexus-* asan-* pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
