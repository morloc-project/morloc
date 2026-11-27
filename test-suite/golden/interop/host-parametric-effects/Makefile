all:
	: > obs.txt
	rm -f *.err cnt-*
	morloc make -o main-py main-py.loc 2>> build.err || echo "--- main-py --- BUILD FAILED" >> obs.txt
	echo "--- main-py mapUnforced ---" >> obs.txt
	[ -x main-py ] && ./main-py mapUnforced $$PWD/cnt-main-py-mapUnforced >> obs.txt 2>> obs.err || true
	echo "--- main-py mapForcedTwice ---" >> obs.txt
	[ -x main-py ] && ./main-py mapForcedTwice $$PWD/cnt-main-py-mapForcedTwice >> obs.txt 2>> obs.err || true
	echo "--- main-py passUnforced ---" >> obs.txt
	[ -x main-py ] && ./main-py passUnforced $$PWD/cnt-main-py-passUnforced >> obs.txt 2>> obs.err || true
	echo "--- main-py passForcedTwice ---" >> obs.txt
	[ -x main-py ] && ./main-py passForcedTwice $$PWD/cnt-main-py-passForcedTwice >> obs.txt 2>> obs.err || true
	morloc make -o main-cpp main-cpp.loc 2>> build.err || echo "--- main-cpp --- BUILD FAILED" >> obs.txt
	echo "--- main-cpp mapUnforced ---" >> obs.txt
	[ -x main-cpp ] && ./main-cpp mapUnforced $$PWD/cnt-main-cpp-mapUnforced >> obs.txt 2>> obs.err || true
	echo "--- main-cpp mapForcedTwice ---" >> obs.txt
	[ -x main-cpp ] && ./main-cpp mapForcedTwice $$PWD/cnt-main-cpp-mapForcedTwice >> obs.txt 2>> obs.err || true
	echo "--- main-cpp passUnforced ---" >> obs.txt
	[ -x main-cpp ] && ./main-cpp passUnforced $$PWD/cnt-main-cpp-passUnforced >> obs.txt 2>> obs.err || true
	echo "--- main-cpp passForcedTwice ---" >> obs.txt
	[ -x main-cpp ] && ./main-cpp passForcedTwice $$PWD/cnt-main-cpp-passForcedTwice >> obs.txt 2>> obs.err || true
	morloc make -o main-rust main-rust.loc 2>> build.err || echo "--- main-rust --- BUILD FAILED" >> obs.txt
	echo "--- main-rust mapUnforced ---" >> obs.txt
	[ -x main-rust ] && ./main-rust mapUnforced $$PWD/cnt-main-rust-mapUnforced >> obs.txt 2>> obs.err || true
	echo "--- main-rust mapForcedTwice ---" >> obs.txt
	[ -x main-rust ] && ./main-rust mapForcedTwice $$PWD/cnt-main-rust-mapForcedTwice >> obs.txt 2>> obs.err || true
	echo "--- main-rust passUnforced ---" >> obs.txt
	[ -x main-rust ] && ./main-rust passUnforced $$PWD/cnt-main-rust-passUnforced >> obs.txt 2>> obs.err || true
	echo "--- main-rust passForcedTwice ---" >> obs.txt
	[ -x main-rust ] && ./main-rust passForcedTwice $$PWD/cnt-main-rust-passForcedTwice >> obs.txt 2>> obs.err || true
	morloc make -o main-r main-r.loc 2>> build.err || echo "--- main-r --- BUILD FAILED" >> obs.txt
	echo "--- main-r mapUnforced ---" >> obs.txt
	[ -x main-r ] && ./main-r mapUnforced $$PWD/cnt-main-r-mapUnforced >> obs.txt 2>> obs.err || true
	echo "--- main-r mapForcedTwice ---" >> obs.txt
	[ -x main-r ] && ./main-r mapForcedTwice $$PWD/cnt-main-r-mapForcedTwice >> obs.txt 2>> obs.err || true
	echo "--- main-r passUnforced ---" >> obs.txt
	[ -x main-r ] && ./main-r passUnforced $$PWD/cnt-main-r-passUnforced >> obs.txt 2>> obs.err || true
	echo "--- main-r passForcedTwice ---" >> obs.txt
	[ -x main-r ] && ./main-r passForcedTwice $$PWD/cnt-main-r-passForcedTwice >> obs.txt 2>> obs.err || true

clean:
	rm -rf main-py main-cpp main-rust main-r __pycache__ *.err cnt-*
	rm -rf *-build *build.tmp*
