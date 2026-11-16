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
	for l in cpp rust; do if morloc make -o swapped-$$l swapped-$$l.loc 2> build-swapped-$$l.err; then echo UNEXPECTED >> obs.txt; elif grep -q 'at other arguments than its own' build-swapped-$$l.err; then echo REFUSED >> obs.txt; else echo UNEXPECTED >> obs.txt; fi; done
	if timeout 120 morloc make -o nest-cpp nest-cpp.loc 2> build-nest-cpp.err; then echo UNEXPECTED >> obs.txt; elif grep -q 'does not terminate' build-nest-cpp.err; then echo REFUSED >> obs.txt; else echo UNEXPECTED >> obs.txt; fi
	for l in cpp rust; do if morloc make -o deep-$$l deep-$$l.loc 2> build-deep-$$l.err; then echo UNEXPECTED >> obs.txt; elif grep -q 'levels deep' build-deep-$$l.err; then echo REFUSED >> obs.txt; else echo UNEXPECTED >> obs.txt; fi; done
	for l in cpp rust; do if morloc make -o deep-user-$$l deep-user-$$l.loc 2> build-deep-user-$$l.err; then echo UNEXPECTED >> obs.txt; elif grep -q 'levels deep' build-deep-user-$$l.err; then echo REFUSED >> obs.txt; else echo UNEXPECTED >> obs.txt; fi; done

clean:
	rm -rf nexus-cpp nexus-rust swapped-cpp swapped-rust nest-cpp deep-cpp deep-rust deep-user-cpp deep-user-rust pools *err *-build *build.tmp*
