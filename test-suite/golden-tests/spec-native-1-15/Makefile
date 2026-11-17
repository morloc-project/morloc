all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus readPt '{"fst":12,"snd":5}' >> obs.txt 2>> obs.err
	./nexus mkPt 7 >> obs.txt 2>> obs.err
	./nexus readPts '[{"fst":12,"snd":5},{"fst":1,"snd":1}]' >> obs.txt 2>> obs.err
	./nexus readOpt '{"fst":12,"snd":5}' >> obs.txt 2>> obs.err
	./nexus readOpt null >> obs.txt 2>> obs.err
	./nexus readW '{"fst":[12,3],"snd":[4,5]}' >> obs.txt 2>> obs.err
	morloc make -o nexus-rust main-rust.loc 2> build-rust.err
	./nexus-rust readPt '{"fst":12,"snd":5}' >> obs.txt 2>> obs.err
	./nexus-rust mkPt 7 >> obs.txt 2>> obs.err
	./nexus-rust readPts '[{"fst":12,"snd":5},{"fst":1,"snd":1}]' >> obs.txt 2>> obs.err
	./nexus-rust readOpt '{"fst":12,"snd":5}' >> obs.txt 2>> obs.err
	./nexus-rust readOpt null >> obs.txt 2>> obs.err
	./nexus-rust readW '{"fst":[12,3],"snd":[4,5]}' >> obs.txt 2>> obs.err
	if morloc make -o nexus-wide wide-rust.loc 2> build-wide.err; then echo UNEXPECTED >> obs.txt; elif grep -q "at most 8 fields" build-wide.err; then echo REFUSED >> obs.txt; else echo UNEXPECTED >> obs.txt; fi

clean:
	rm -rf nexus nexus-rust nexus-wide pools __pycache__ *err *-build *build.tmp*
