all:
	rm -f *.err obs.txt
	morloc make -o nexus-cpp main-cpp.loc 2> build-cpp.err
	./nexus-cpp depth 5 >> obs.txt 2>> obs.err
	timeout 60s ./nexus-cpp deep 1000000 >> obs.txt 2>> obs.err
	./nexus-cpp trip '{"val":1,"sub":{"val":2,"sub":null}}' >> obs.txt 2>> obs.err
	./nexus-cpp strTrip '{"val":"a","sub":{"val":"b","sub":null}}' >> obs.txt 2>> obs.err
	morloc make -o nexus-rust main-rust.loc 2> build-rust.err
	./nexus-rust depth 5 >> obs.txt 2>> obs.err
	timeout 60s ./nexus-rust deep 1000000 >> obs.txt 2>> obs.err
	./nexus-rust trip '{"val":1,"sub":{"val":2,"sub":null}}' >> obs.txt 2>> obs.err
	./nexus-rust strTrip '{"val":"a","sub":{"val":"b","sub":null}}' >> obs.txt 2>> obs.err
	morloc make -o swapped-cpp swapped-cpp.loc 2> build-swapped-cpp.err
	./swapped-cpp trip '{"val":1,"sub":{"val":"a","sub":{"val":2,"sub":null}}}' >> obs.txt 2>> obs.err
	morloc make -o swapped-rust swapped-rust.loc 2> build-swapped-rust.err
	./swapped-rust trip '{"val":1,"sub":{"val":"a","sub":{"val":2,"sub":null}}}' >> obs.txt 2>> obs.err
	if morloc make -o swapped-fn-rust swapped-fn-rust.loc 2> build-swapped-fn-rust.err; then echo UNEXPECTED >> obs.txt; elif grep -q 'other arguments' build-swapped-fn-rust.err; then echo REFUSED >> obs.txt; else echo UNEXPECTED >> obs.txt; fi
	if timeout 120 morloc make -o nest-cpp nest-cpp.loc 2> build-nest-cpp.err; then echo UNEXPECTED >> obs.txt; elif grep -q 'no finite wire form' build-nest-cpp.err; then echo REFUSED >> obs.txt; else echo UNEXPECTED >> obs.txt; fi
	morloc make -o deep-cpp deep-cpp.loc 2> build-deep-cpp.err
	./deep-cpp trip '{"val":{"val":{"val":{"val":{"val":{"val":{"val":1,"sub":{"val":2,"sub":null}},"sub":null},"sub":null},"sub":null},"sub":null},"sub":null},"sub":null}' >> obs.txt 2>> obs.err
	morloc make -o deep-rust deep-rust.loc 2> build-deep-rust.err
	./deep-rust trip '{"val":{"val":{"val":{"val":{"val":{"val":{"val":1,"sub":{"val":2,"sub":null}},"sub":null},"sub":null},"sub":null},"sub":null},"sub":null},"sub":null}' >> obs.txt 2>> obs.err
	morloc make -o deep-user-cpp deep-user-cpp.loc 2> build-deep-user-cpp.err
	./deep-user-cpp trip '{"val":{"val":{"val":{"val":{"val":{"val":{"val":1,"next":{"val":2,"next":null}},"next":null},"next":null},"next":null},"next":null},"next":null},"next":null}' >> obs.txt 2>> obs.err
	morloc make -o deep-user-rust deep-user-rust.loc 2> build-deep-user-rust.err
	./deep-user-rust trip '{"val":{"val":{"val":{"val":{"val":{"val":{"val":1,"next":{"val":2,"next":null}},"next":null},"next":null},"next":null},"next":null},"next":null},"next":null}' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus-cpp nexus-rust swapped-cpp swapped-rust swapped-fn-rust __pycache__ nest-cpp deep-cpp deep-rust deep-user-cpp deep-user-rust pools *err *-build *build.tmp*
