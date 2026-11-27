PROGRAMS = pair-cpp-r pair-cpp-rust pair-r-cpp pair-r-rust pair-rust-cpp pair-rust-r

all:
	: > obs.txt
	rm -f *.err cnt-*
	morloc make -o pair-cpp-r pair-cpp-r.loc 2>> build.err || echo "--- pair-cpp-r --- BUILD FAILED" >> obs.txt
	echo "--- pair-cpp-r rootResult ---" >> obs.txt
	[ -x pair-cpp-r ] && ./pair-cpp-r rootResult $$PWD/cnt-pair-cpp-r-rootResult >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-r rootArg ---" >> obs.txt
	[ -x pair-cpp-r ] && ./pair-cpp-r rootArg $$PWD/cnt-pair-cpp-r-rootArg >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-r recordField ---" >> obs.txt
	[ -x pair-cpp-r ] && ./pair-cpp-r recordField $$PWD/cnt-pair-cpp-r-recordField >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-r listElem ---" >> obs.txt
	[ -x pair-cpp-r ] && ./pair-cpp-r listElem $$PWD/cnt-pair-cpp-r-listElem >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-r callback ---" >> obs.txt
	[ -x pair-cpp-r ] && ./pair-cpp-r callback $$PWD/cnt-pair-cpp-r-callback >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-r pinned ---" >> obs.txt
	[ -x pair-cpp-r ] && ./pair-cpp-r pinned $$PWD/cnt-pair-cpp-r-pinned >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-r relay ---" >> obs.txt
	[ -x pair-cpp-r ] && ./pair-cpp-r relay $$PWD/cnt-pair-cpp-r-relay >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-r closureResult ---" >> obs.txt
	[ -x pair-cpp-r ] && ./pair-cpp-r closureResult $$PWD/cnt-pair-cpp-r-closureResult >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-r nestedSusp ---" >> obs.txt
	[ -x pair-cpp-r ] && ./pair-cpp-r nestedSusp $$PWD/cnt-pair-cpp-r-nestedSusp >> obs.txt 2>> obs.err || true
	morloc make -o pair-cpp-rust pair-cpp-rust.loc 2>> build.err || echo "--- pair-cpp-rust --- BUILD FAILED" >> obs.txt
	echo "--- pair-cpp-rust rootResult ---" >> obs.txt
	[ -x pair-cpp-rust ] && ./pair-cpp-rust rootResult $$PWD/cnt-pair-cpp-rust-rootResult >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-rust rootArg ---" >> obs.txt
	[ -x pair-cpp-rust ] && ./pair-cpp-rust rootArg $$PWD/cnt-pair-cpp-rust-rootArg >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-rust recordField ---" >> obs.txt
	[ -x pair-cpp-rust ] && ./pair-cpp-rust recordField $$PWD/cnt-pair-cpp-rust-recordField >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-rust listElem ---" >> obs.txt
	[ -x pair-cpp-rust ] && ./pair-cpp-rust listElem $$PWD/cnt-pair-cpp-rust-listElem >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-rust callback ---" >> obs.txt
	[ -x pair-cpp-rust ] && ./pair-cpp-rust callback $$PWD/cnt-pair-cpp-rust-callback >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-rust pinned ---" >> obs.txt
	[ -x pair-cpp-rust ] && ./pair-cpp-rust pinned $$PWD/cnt-pair-cpp-rust-pinned >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-rust relay ---" >> obs.txt
	[ -x pair-cpp-rust ] && ./pair-cpp-rust relay $$PWD/cnt-pair-cpp-rust-relay >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-rust closureResult ---" >> obs.txt
	[ -x pair-cpp-rust ] && ./pair-cpp-rust closureResult $$PWD/cnt-pair-cpp-rust-closureResult >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-rust nestedSusp ---" >> obs.txt
	[ -x pair-cpp-rust ] && ./pair-cpp-rust nestedSusp $$PWD/cnt-pair-cpp-rust-nestedSusp >> obs.txt 2>> obs.err || true
	morloc make -o pair-r-cpp pair-r-cpp.loc 2>> build.err || echo "--- pair-r-cpp --- BUILD FAILED" >> obs.txt
	echo "--- pair-r-cpp rootResult ---" >> obs.txt
	[ -x pair-r-cpp ] && ./pair-r-cpp rootResult $$PWD/cnt-pair-r-cpp-rootResult >> obs.txt 2>> obs.err || true
	echo "--- pair-r-cpp rootArg ---" >> obs.txt
	[ -x pair-r-cpp ] && ./pair-r-cpp rootArg $$PWD/cnt-pair-r-cpp-rootArg >> obs.txt 2>> obs.err || true
	echo "--- pair-r-cpp recordField ---" >> obs.txt
	[ -x pair-r-cpp ] && ./pair-r-cpp recordField $$PWD/cnt-pair-r-cpp-recordField >> obs.txt 2>> obs.err || true
	echo "--- pair-r-cpp listElem ---" >> obs.txt
	[ -x pair-r-cpp ] && ./pair-r-cpp listElem $$PWD/cnt-pair-r-cpp-listElem >> obs.txt 2>> obs.err || true
	echo "--- pair-r-cpp callback ---" >> obs.txt
	[ -x pair-r-cpp ] && ./pair-r-cpp callback $$PWD/cnt-pair-r-cpp-callback >> obs.txt 2>> obs.err || true
	echo "--- pair-r-cpp pinned ---" >> obs.txt
	[ -x pair-r-cpp ] && ./pair-r-cpp pinned $$PWD/cnt-pair-r-cpp-pinned >> obs.txt 2>> obs.err || true
	echo "--- pair-r-cpp relay ---" >> obs.txt
	[ -x pair-r-cpp ] && ./pair-r-cpp relay $$PWD/cnt-pair-r-cpp-relay >> obs.txt 2>> obs.err || true
	echo "--- pair-r-cpp closureResult ---" >> obs.txt
	[ -x pair-r-cpp ] && ./pair-r-cpp closureResult $$PWD/cnt-pair-r-cpp-closureResult >> obs.txt 2>> obs.err || true
	echo "--- pair-r-cpp nestedSusp ---" >> obs.txt
	[ -x pair-r-cpp ] && ./pair-r-cpp nestedSusp $$PWD/cnt-pair-r-cpp-nestedSusp >> obs.txt 2>> obs.err || true
	morloc make -o pair-r-rust pair-r-rust.loc 2>> build.err || echo "--- pair-r-rust --- BUILD FAILED" >> obs.txt
	echo "--- pair-r-rust rootResult ---" >> obs.txt
	[ -x pair-r-rust ] && ./pair-r-rust rootResult $$PWD/cnt-pair-r-rust-rootResult >> obs.txt 2>> obs.err || true
	echo "--- pair-r-rust rootArg ---" >> obs.txt
	[ -x pair-r-rust ] && ./pair-r-rust rootArg $$PWD/cnt-pair-r-rust-rootArg >> obs.txt 2>> obs.err || true
	echo "--- pair-r-rust recordField ---" >> obs.txt
	[ -x pair-r-rust ] && ./pair-r-rust recordField $$PWD/cnt-pair-r-rust-recordField >> obs.txt 2>> obs.err || true
	echo "--- pair-r-rust listElem ---" >> obs.txt
	[ -x pair-r-rust ] && ./pair-r-rust listElem $$PWD/cnt-pair-r-rust-listElem >> obs.txt 2>> obs.err || true
	echo "--- pair-r-rust callback ---" >> obs.txt
	[ -x pair-r-rust ] && ./pair-r-rust callback $$PWD/cnt-pair-r-rust-callback >> obs.txt 2>> obs.err || true
	echo "--- pair-r-rust pinned ---" >> obs.txt
	[ -x pair-r-rust ] && ./pair-r-rust pinned $$PWD/cnt-pair-r-rust-pinned >> obs.txt 2>> obs.err || true
	echo "--- pair-r-rust relay ---" >> obs.txt
	[ -x pair-r-rust ] && ./pair-r-rust relay $$PWD/cnt-pair-r-rust-relay >> obs.txt 2>> obs.err || true
	echo "--- pair-r-rust closureResult ---" >> obs.txt
	[ -x pair-r-rust ] && ./pair-r-rust closureResult $$PWD/cnt-pair-r-rust-closureResult >> obs.txt 2>> obs.err || true
	echo "--- pair-r-rust nestedSusp ---" >> obs.txt
	[ -x pair-r-rust ] && ./pair-r-rust nestedSusp $$PWD/cnt-pair-r-rust-nestedSusp >> obs.txt 2>> obs.err || true
	morloc make -o pair-rust-cpp pair-rust-cpp.loc 2>> build.err || echo "--- pair-rust-cpp --- BUILD FAILED" >> obs.txt
	echo "--- pair-rust-cpp rootResult ---" >> obs.txt
	[ -x pair-rust-cpp ] && ./pair-rust-cpp rootResult $$PWD/cnt-pair-rust-cpp-rootResult >> obs.txt 2>> obs.err || true
	echo "--- pair-rust-cpp rootArg ---" >> obs.txt
	[ -x pair-rust-cpp ] && ./pair-rust-cpp rootArg $$PWD/cnt-pair-rust-cpp-rootArg >> obs.txt 2>> obs.err || true
	echo "--- pair-rust-cpp recordField ---" >> obs.txt
	[ -x pair-rust-cpp ] && ./pair-rust-cpp recordField $$PWD/cnt-pair-rust-cpp-recordField >> obs.txt 2>> obs.err || true
	echo "--- pair-rust-cpp listElem ---" >> obs.txt
	[ -x pair-rust-cpp ] && ./pair-rust-cpp listElem $$PWD/cnt-pair-rust-cpp-listElem >> obs.txt 2>> obs.err || true
	echo "--- pair-rust-cpp callback ---" >> obs.txt
	[ -x pair-rust-cpp ] && ./pair-rust-cpp callback $$PWD/cnt-pair-rust-cpp-callback >> obs.txt 2>> obs.err || true
	echo "--- pair-rust-cpp pinned ---" >> obs.txt
	[ -x pair-rust-cpp ] && ./pair-rust-cpp pinned $$PWD/cnt-pair-rust-cpp-pinned >> obs.txt 2>> obs.err || true
	echo "--- pair-rust-cpp relay ---" >> obs.txt
	[ -x pair-rust-cpp ] && ./pair-rust-cpp relay $$PWD/cnt-pair-rust-cpp-relay >> obs.txt 2>> obs.err || true
	echo "--- pair-rust-cpp closureResult ---" >> obs.txt
	[ -x pair-rust-cpp ] && ./pair-rust-cpp closureResult $$PWD/cnt-pair-rust-cpp-closureResult >> obs.txt 2>> obs.err || true
	echo "--- pair-rust-cpp nestedSusp ---" >> obs.txt
	[ -x pair-rust-cpp ] && ./pair-rust-cpp nestedSusp $$PWD/cnt-pair-rust-cpp-nestedSusp >> obs.txt 2>> obs.err || true
	morloc make -o pair-rust-r pair-rust-r.loc 2>> build.err || echo "--- pair-rust-r --- BUILD FAILED" >> obs.txt
	echo "--- pair-rust-r rootResult ---" >> obs.txt
	[ -x pair-rust-r ] && ./pair-rust-r rootResult $$PWD/cnt-pair-rust-r-rootResult >> obs.txt 2>> obs.err || true
	echo "--- pair-rust-r rootArg ---" >> obs.txt
	[ -x pair-rust-r ] && ./pair-rust-r rootArg $$PWD/cnt-pair-rust-r-rootArg >> obs.txt 2>> obs.err || true
	echo "--- pair-rust-r recordField ---" >> obs.txt
	[ -x pair-rust-r ] && ./pair-rust-r recordField $$PWD/cnt-pair-rust-r-recordField >> obs.txt 2>> obs.err || true
	echo "--- pair-rust-r listElem ---" >> obs.txt
	[ -x pair-rust-r ] && ./pair-rust-r listElem $$PWD/cnt-pair-rust-r-listElem >> obs.txt 2>> obs.err || true
	echo "--- pair-rust-r callback ---" >> obs.txt
	[ -x pair-rust-r ] && ./pair-rust-r callback $$PWD/cnt-pair-rust-r-callback >> obs.txt 2>> obs.err || true
	echo "--- pair-rust-r pinned ---" >> obs.txt
	[ -x pair-rust-r ] && ./pair-rust-r pinned $$PWD/cnt-pair-rust-r-pinned >> obs.txt 2>> obs.err || true
	echo "--- pair-rust-r relay ---" >> obs.txt
	[ -x pair-rust-r ] && ./pair-rust-r relay $$PWD/cnt-pair-rust-r-relay >> obs.txt 2>> obs.err || true
	echo "--- pair-rust-r closureResult ---" >> obs.txt
	[ -x pair-rust-r ] && ./pair-rust-r closureResult $$PWD/cnt-pair-rust-r-closureResult >> obs.txt 2>> obs.err || true
	echo "--- pair-rust-r nestedSusp ---" >> obs.txt
	[ -x pair-rust-r ] && ./pair-rust-r nestedSusp $$PWD/cnt-pair-rust-r-nestedSusp >> obs.txt 2>> obs.err || true

clean:
	rm -rf $(PROGRAMS) __pycache__ *.err cnt-*
	rm -rf *-build *build.tmp*
