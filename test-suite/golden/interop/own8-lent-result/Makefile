all:
	rm -f *.err obs.txt
	morloc make -o nexus-cpp main-cpp.loc 2>> build.err
	CXX="$${CXX:-g++} -fsanitize=address,undefined -fno-sanitize-recover=all" morloc make -o asan-cpp main-cpp.loc 2>> build.err
	MORLOC_LANG_PARAMS=rust:lto=off morloc make -o nexus-rs main-rs.loc 2>> build.err
	for l in cpp rs; do for f in loopPart loopWhole loopFresh keepsLent keepsFresh movesAfter movesWith tmpArg strLit otherMoves returnedMoves mixed scalarArg takeAfter takeFirst condPart condFresh condLoop condKeeps condMoves condWhole condTwo condTwoMoves; do echo "$$l $$f $$(./nexus-$$l $$f 1 2>> obs.err || echo failed)" >> obs.txt; done; done
	for f in loopPart loopWhole loopFresh keepsLent keepsFresh movesAfter movesWith tmpArg strLit otherMoves returnedMoves mixed scalarArg takeAfter takeFirst condPart condFresh condLoop condKeeps condMoves condWhole condTwo condTwoMoves; do echo "asan $$f $$(ASAN_OPTIONS=detect_leaks=0 ./asan-cpp $$f 1 2>> obs.err || echo failed)" >> obs.txt; done

clean:
	rm -rf nexus-* asan-* pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
