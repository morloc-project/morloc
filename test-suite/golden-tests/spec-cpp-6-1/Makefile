all:
	rm -f *.err obs.txt
	morloc make -o bad-arm bad-arm.loc 2> build-arm.err && echo "UNEXPECTED_SUCCESS arm" >> obs.txt || (grep -q "discards qualifiers\|read-only\|not marked const\|const value\|const-qualified" build-arm.err && echo "rejected arm" || echo "UNEXPECTED_ERROR arm") >> obs.txt
	morloc make -o bad-raw bad-raw.loc 2> build-raw.err && echo "UNEXPECTED_SUCCESS raw" >> obs.txt || (grep -q "discards qualifiers\|read-only\|not marked const\|const value\|const-qualified" build-raw.err && echo "rejected raw" || echo "UNEXPECTED_ERROR raw") >> obs.txt
	morloc make -o bad-visit bad-visit.loc 2> build-visit.err && echo "UNEXPECTED_SUCCESS visit" >> obs.txt || (grep -q "discards qualifiers\|read-only\|not marked const\|const value\|const-qualified" build-visit.err && echo "rejected visit" || echo "UNEXPECTED_ERROR visit") >> obs.txt
	morloc make -o bad-tail bad-tail.loc 2> build-tail.err && echo "UNEXPECTED_SUCCESS tail" >> obs.txt || (grep -q "discards qualifiers\|read-only\|not marked const\|const value\|const-qualified" build-tail.err && echo "rejected tail" || echo "UNEXPECTED_ERROR tail") >> obs.txt
	morloc make -o bad-mapped bad-mapped.loc 2> build-mapped.err && echo "UNEXPECTED_SUCCESS mapped" >> obs.txt || (grep -q "CPP-6" build-mapped.err && echo "rejected mapped" || echo "UNEXPECTED_ERROR mapped") >> obs.txt
	morloc make -o bad-rec bad-rec.loc 2> build-rec.err && echo "UNEXPECTED_SUCCESS rec" >> obs.txt || (grep -q "CPP-6" build-rec.err && echo "rejected rec" || echo "UNEXPECTED_ERROR rec") >> obs.txt
	morloc make -o bad-unique bad-unique.loc 2> build-unique.err && echo "UNEXPECTED_SUCCESS unique" >> obs.txt || (grep -q "CPP-6" build-unique.err && echo "rejected unique" || echo "UNEXPECTED_ERROR unique") >> obs.txt
	morloc make -o nexus ok.loc 2> build.err
	./nexus boxAfter 3 >> obs.txt 2>> obs.err
	./nexus shapeAfter 3 >> obs.txt 2>> obs.err
	./nexus listLen 4 >> obs.txt 2>> obs.err
	./nexus listShare 4 >> obs.txt 2>> obs.err
	./nexus lenArg '{"head":1,"tail":{"head":2,"tail":null}}' >> obs.txt 2>> obs.err
	./nexus shapeArg '{"Circle":[[1,1]]}' >> obs.txt 2>> obs.err
	./nexus shapeOut 2 >> obs.txt 2>> obs.err
	./nexus taken 5 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus bad-arm bad-raw bad-visit bad-tail bad-mapped bad-rec bad-unique pools __pycache__
	rm -f *.err
	rm -rf *-build *build.tmp*
