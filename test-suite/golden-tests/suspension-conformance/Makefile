PROGRAMS = pair-py-py pair-py-cpp pair-py-r pair-py-rust nested-py-rust pair-cpp-py pair-cpp-cpp pair-cpp-r pair-cpp-rust nested-cpp-rust pair-r-py pair-r-cpp pair-r-r pair-r-rust nested-r-rust pair-rust-py nested-rust-py pair-rust-cpp nested-rust-cpp pair-rust-r nested-rust-r pair-rust-rust nested-rust-rust

all:
	: > obs.txt
	rm -f *.err cnt-*
	morloc make -o pair-py-py pair-py-py.loc 2>> build.err || echo "--- pair-py-py --- BUILD FAILED" >> obs.txt
	echo "--- pair-py-py rootResult ---" >> obs.txt
	[ -x pair-py-py ] && ./pair-py-py rootResult $$PWD/cnt-pair-py-py-rootResult >> obs.txt 2>> obs.err || true
	echo "--- pair-py-py rootArg ---" >> obs.txt
	[ -x pair-py-py ] && ./pair-py-py rootArg $$PWD/cnt-pair-py-py-rootArg >> obs.txt 2>> obs.err || true
	echo "--- pair-py-py recordField ---" >> obs.txt
	[ -x pair-py-py ] && ./pair-py-py recordField $$PWD/cnt-pair-py-py-recordField >> obs.txt 2>> obs.err || true
	echo "--- pair-py-py listElem ---" >> obs.txt
	[ -x pair-py-py ] && ./pair-py-py listElem $$PWD/cnt-pair-py-py-listElem >> obs.txt 2>> obs.err || true
	echo "--- pair-py-py callback ---" >> obs.txt
	[ -x pair-py-py ] && ./pair-py-py callback $$PWD/cnt-pair-py-py-callback >> obs.txt 2>> obs.err || true
	echo "--- pair-py-py pinned ---" >> obs.txt
	[ -x pair-py-py ] && ./pair-py-py pinned $$PWD/cnt-pair-py-py-pinned >> obs.txt 2>> obs.err || true
	echo "--- pair-py-py relay ---" >> obs.txt
	[ -x pair-py-py ] && ./pair-py-py relay $$PWD/cnt-pair-py-py-relay >> obs.txt 2>> obs.err || true
	echo "--- pair-py-py closureResult ---" >> obs.txt
	[ -x pair-py-py ] && ./pair-py-py closureResult $$PWD/cnt-pair-py-py-closureResult >> obs.txt 2>> obs.err || true
	echo "--- pair-py-py nestedSusp ---" >> obs.txt
	[ -x pair-py-py ] && ./pair-py-py nestedSusp $$PWD/cnt-pair-py-py-nestedSusp >> obs.txt 2>> obs.err || true
	morloc make -o pair-py-cpp pair-py-cpp.loc 2>> build.err || echo "--- pair-py-cpp --- BUILD FAILED" >> obs.txt
	echo "--- pair-py-cpp rootResult ---" >> obs.txt
	[ -x pair-py-cpp ] && ./pair-py-cpp rootResult $$PWD/cnt-pair-py-cpp-rootResult >> obs.txt 2>> obs.err || true
	echo "--- pair-py-cpp rootArg ---" >> obs.txt
	[ -x pair-py-cpp ] && ./pair-py-cpp rootArg $$PWD/cnt-pair-py-cpp-rootArg >> obs.txt 2>> obs.err || true
	echo "--- pair-py-cpp recordField ---" >> obs.txt
	[ -x pair-py-cpp ] && ./pair-py-cpp recordField $$PWD/cnt-pair-py-cpp-recordField >> obs.txt 2>> obs.err || true
	echo "--- pair-py-cpp listElem ---" >> obs.txt
	[ -x pair-py-cpp ] && ./pair-py-cpp listElem $$PWD/cnt-pair-py-cpp-listElem >> obs.txt 2>> obs.err || true
	echo "--- pair-py-cpp callback ---" >> obs.txt
	[ -x pair-py-cpp ] && ./pair-py-cpp callback $$PWD/cnt-pair-py-cpp-callback >> obs.txt 2>> obs.err || true
	echo "--- pair-py-cpp pinned ---" >> obs.txt
	[ -x pair-py-cpp ] && ./pair-py-cpp pinned $$PWD/cnt-pair-py-cpp-pinned >> obs.txt 2>> obs.err || true
	echo "--- pair-py-cpp relay ---" >> obs.txt
	[ -x pair-py-cpp ] && ./pair-py-cpp relay $$PWD/cnt-pair-py-cpp-relay >> obs.txt 2>> obs.err || true
	echo "--- pair-py-cpp closureResult ---" >> obs.txt
	[ -x pair-py-cpp ] && ./pair-py-cpp closureResult $$PWD/cnt-pair-py-cpp-closureResult >> obs.txt 2>> obs.err || true
	echo "--- pair-py-cpp nestedSusp ---" >> obs.txt
	[ -x pair-py-cpp ] && ./pair-py-cpp nestedSusp $$PWD/cnt-pair-py-cpp-nestedSusp >> obs.txt 2>> obs.err || true
	morloc make -o pair-py-r pair-py-r.loc 2>> build.err || echo "--- pair-py-r --- BUILD FAILED" >> obs.txt
	echo "--- pair-py-r rootResult ---" >> obs.txt
	[ -x pair-py-r ] && ./pair-py-r rootResult $$PWD/cnt-pair-py-r-rootResult >> obs.txt 2>> obs.err || true
	echo "--- pair-py-r rootArg ---" >> obs.txt
	[ -x pair-py-r ] && ./pair-py-r rootArg $$PWD/cnt-pair-py-r-rootArg >> obs.txt 2>> obs.err || true
	echo "--- pair-py-r recordField ---" >> obs.txt
	[ -x pair-py-r ] && ./pair-py-r recordField $$PWD/cnt-pair-py-r-recordField >> obs.txt 2>> obs.err || true
	echo "--- pair-py-r listElem ---" >> obs.txt
	[ -x pair-py-r ] && ./pair-py-r listElem $$PWD/cnt-pair-py-r-listElem >> obs.txt 2>> obs.err || true
	echo "--- pair-py-r callback ---" >> obs.txt
	[ -x pair-py-r ] && ./pair-py-r callback $$PWD/cnt-pair-py-r-callback >> obs.txt 2>> obs.err || true
	echo "--- pair-py-r pinned ---" >> obs.txt
	[ -x pair-py-r ] && ./pair-py-r pinned $$PWD/cnt-pair-py-r-pinned >> obs.txt 2>> obs.err || true
	echo "--- pair-py-r relay ---" >> obs.txt
	[ -x pair-py-r ] && ./pair-py-r relay $$PWD/cnt-pair-py-r-relay >> obs.txt 2>> obs.err || true
	echo "--- pair-py-r closureResult ---" >> obs.txt
	[ -x pair-py-r ] && ./pair-py-r closureResult $$PWD/cnt-pair-py-r-closureResult >> obs.txt 2>> obs.err || true
	echo "--- pair-py-r nestedSusp ---" >> obs.txt
	[ -x pair-py-r ] && ./pair-py-r nestedSusp $$PWD/cnt-pair-py-r-nestedSusp >> obs.txt 2>> obs.err || true
	morloc make -o pair-py-rust pair-py-rust.loc 2>> build.err || echo "--- pair-py-rust --- BUILD FAILED" >> obs.txt
	echo "--- pair-py-rust rootResult ---" >> obs.txt
	[ -x pair-py-rust ] && ./pair-py-rust rootResult $$PWD/cnt-pair-py-rust-rootResult >> obs.txt 2>> obs.err || true
	echo "--- pair-py-rust rootArg ---" >> obs.txt
	[ -x pair-py-rust ] && ./pair-py-rust rootArg $$PWD/cnt-pair-py-rust-rootArg >> obs.txt 2>> obs.err || true
	echo "--- pair-py-rust recordField ---" >> obs.txt
	[ -x pair-py-rust ] && ./pair-py-rust recordField $$PWD/cnt-pair-py-rust-recordField >> obs.txt 2>> obs.err || true
	echo "--- pair-py-rust listElem ---" >> obs.txt
	[ -x pair-py-rust ] && ./pair-py-rust listElem $$PWD/cnt-pair-py-rust-listElem >> obs.txt 2>> obs.err || true
	echo "--- pair-py-rust callback ---" >> obs.txt
	[ -x pair-py-rust ] && ./pair-py-rust callback $$PWD/cnt-pair-py-rust-callback >> obs.txt 2>> obs.err || true
	echo "--- pair-py-rust pinned ---" >> obs.txt
	[ -x pair-py-rust ] && ./pair-py-rust pinned $$PWD/cnt-pair-py-rust-pinned >> obs.txt 2>> obs.err || true
	echo "--- pair-py-rust relay ---" >> obs.txt
	[ -x pair-py-rust ] && ./pair-py-rust relay $$PWD/cnt-pair-py-rust-relay >> obs.txt 2>> obs.err || true
	morloc make -o nested-py-rust nested-py-rust.loc 2>> build.err || echo "--- nested-py-rust --- BUILD FAILED" >> obs.txt
	echo "--- nested-py-rust closureResult ---" >> obs.txt
	[ -x nested-py-rust ] && ./nested-py-rust closureResult $$PWD/cnt-nested-py-rust-closureResult >> obs.txt 2>> obs.err || true
	echo "--- nested-py-rust nestedSusp ---" >> obs.txt
	[ -x nested-py-rust ] && ./nested-py-rust nestedSusp $$PWD/cnt-nested-py-rust-nestedSusp >> obs.txt 2>> obs.err || true
	morloc make -o pair-cpp-py pair-cpp-py.loc 2>> build.err || echo "--- pair-cpp-py --- BUILD FAILED" >> obs.txt
	echo "--- pair-cpp-py rootResult ---" >> obs.txt
	[ -x pair-cpp-py ] && ./pair-cpp-py rootResult $$PWD/cnt-pair-cpp-py-rootResult >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-py rootArg ---" >> obs.txt
	[ -x pair-cpp-py ] && ./pair-cpp-py rootArg $$PWD/cnt-pair-cpp-py-rootArg >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-py recordField ---" >> obs.txt
	[ -x pair-cpp-py ] && ./pair-cpp-py recordField $$PWD/cnt-pair-cpp-py-recordField >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-py listElem ---" >> obs.txt
	[ -x pair-cpp-py ] && ./pair-cpp-py listElem $$PWD/cnt-pair-cpp-py-listElem >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-py callback ---" >> obs.txt
	[ -x pair-cpp-py ] && ./pair-cpp-py callback $$PWD/cnt-pair-cpp-py-callback >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-py pinned ---" >> obs.txt
	[ -x pair-cpp-py ] && ./pair-cpp-py pinned $$PWD/cnt-pair-cpp-py-pinned >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-py relay ---" >> obs.txt
	[ -x pair-cpp-py ] && ./pair-cpp-py relay $$PWD/cnt-pair-cpp-py-relay >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-py closureResult ---" >> obs.txt
	[ -x pair-cpp-py ] && ./pair-cpp-py closureResult $$PWD/cnt-pair-cpp-py-closureResult >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-py nestedSusp ---" >> obs.txt
	[ -x pair-cpp-py ] && ./pair-cpp-py nestedSusp $$PWD/cnt-pair-cpp-py-nestedSusp >> obs.txt 2>> obs.err || true
	morloc make -o pair-cpp-cpp pair-cpp-cpp.loc 2>> build.err || echo "--- pair-cpp-cpp --- BUILD FAILED" >> obs.txt
	echo "--- pair-cpp-cpp rootResult ---" >> obs.txt
	[ -x pair-cpp-cpp ] && ./pair-cpp-cpp rootResult $$PWD/cnt-pair-cpp-cpp-rootResult >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-cpp rootArg ---" >> obs.txt
	[ -x pair-cpp-cpp ] && ./pair-cpp-cpp rootArg $$PWD/cnt-pair-cpp-cpp-rootArg >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-cpp recordField ---" >> obs.txt
	[ -x pair-cpp-cpp ] && ./pair-cpp-cpp recordField $$PWD/cnt-pair-cpp-cpp-recordField >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-cpp listElem ---" >> obs.txt
	[ -x pair-cpp-cpp ] && ./pair-cpp-cpp listElem $$PWD/cnt-pair-cpp-cpp-listElem >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-cpp callback ---" >> obs.txt
	[ -x pair-cpp-cpp ] && ./pair-cpp-cpp callback $$PWD/cnt-pair-cpp-cpp-callback >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-cpp pinned ---" >> obs.txt
	[ -x pair-cpp-cpp ] && ./pair-cpp-cpp pinned $$PWD/cnt-pair-cpp-cpp-pinned >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-cpp relay ---" >> obs.txt
	[ -x pair-cpp-cpp ] && ./pair-cpp-cpp relay $$PWD/cnt-pair-cpp-cpp-relay >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-cpp closureResult ---" >> obs.txt
	[ -x pair-cpp-cpp ] && ./pair-cpp-cpp closureResult $$PWD/cnt-pair-cpp-cpp-closureResult >> obs.txt 2>> obs.err || true
	echo "--- pair-cpp-cpp nestedSusp ---" >> obs.txt
	[ -x pair-cpp-cpp ] && ./pair-cpp-cpp nestedSusp $$PWD/cnt-pair-cpp-cpp-nestedSusp >> obs.txt 2>> obs.err || true
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
	morloc make -o nested-cpp-rust nested-cpp-rust.loc 2>> build.err || echo "--- nested-cpp-rust --- BUILD FAILED" >> obs.txt
	echo "--- nested-cpp-rust closureResult ---" >> obs.txt
	[ -x nested-cpp-rust ] && ./nested-cpp-rust closureResult $$PWD/cnt-nested-cpp-rust-closureResult >> obs.txt 2>> obs.err || true
	echo "--- nested-cpp-rust nestedSusp ---" >> obs.txt
	[ -x nested-cpp-rust ] && ./nested-cpp-rust nestedSusp $$PWD/cnt-nested-cpp-rust-nestedSusp >> obs.txt 2>> obs.err || true
	morloc make -o pair-r-py pair-r-py.loc 2>> build.err || echo "--- pair-r-py --- BUILD FAILED" >> obs.txt
	echo "--- pair-r-py rootResult ---" >> obs.txt
	[ -x pair-r-py ] && ./pair-r-py rootResult $$PWD/cnt-pair-r-py-rootResult >> obs.txt 2>> obs.err || true
	echo "--- pair-r-py rootArg ---" >> obs.txt
	[ -x pair-r-py ] && ./pair-r-py rootArg $$PWD/cnt-pair-r-py-rootArg >> obs.txt 2>> obs.err || true
	echo "--- pair-r-py recordField ---" >> obs.txt
	[ -x pair-r-py ] && ./pair-r-py recordField $$PWD/cnt-pair-r-py-recordField >> obs.txt 2>> obs.err || true
	echo "--- pair-r-py listElem ---" >> obs.txt
	[ -x pair-r-py ] && ./pair-r-py listElem $$PWD/cnt-pair-r-py-listElem >> obs.txt 2>> obs.err || true
	echo "--- pair-r-py callback ---" >> obs.txt
	[ -x pair-r-py ] && ./pair-r-py callback $$PWD/cnt-pair-r-py-callback >> obs.txt 2>> obs.err || true
	echo "--- pair-r-py pinned ---" >> obs.txt
	[ -x pair-r-py ] && ./pair-r-py pinned $$PWD/cnt-pair-r-py-pinned >> obs.txt 2>> obs.err || true
	echo "--- pair-r-py relay ---" >> obs.txt
	[ -x pair-r-py ] && ./pair-r-py relay $$PWD/cnt-pair-r-py-relay >> obs.txt 2>> obs.err || true
	echo "--- pair-r-py closureResult ---" >> obs.txt
	[ -x pair-r-py ] && ./pair-r-py closureResult $$PWD/cnt-pair-r-py-closureResult >> obs.txt 2>> obs.err || true
	echo "--- pair-r-py nestedSusp ---" >> obs.txt
	[ -x pair-r-py ] && ./pair-r-py nestedSusp $$PWD/cnt-pair-r-py-nestedSusp >> obs.txt 2>> obs.err || true
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
	morloc make -o pair-r-r pair-r-r.loc 2>> build.err || echo "--- pair-r-r --- BUILD FAILED" >> obs.txt
	echo "--- pair-r-r rootResult ---" >> obs.txt
	[ -x pair-r-r ] && ./pair-r-r rootResult $$PWD/cnt-pair-r-r-rootResult >> obs.txt 2>> obs.err || true
	echo "--- pair-r-r rootArg ---" >> obs.txt
	[ -x pair-r-r ] && ./pair-r-r rootArg $$PWD/cnt-pair-r-r-rootArg >> obs.txt 2>> obs.err || true
	echo "--- pair-r-r recordField ---" >> obs.txt
	[ -x pair-r-r ] && ./pair-r-r recordField $$PWD/cnt-pair-r-r-recordField >> obs.txt 2>> obs.err || true
	echo "--- pair-r-r listElem ---" >> obs.txt
	[ -x pair-r-r ] && ./pair-r-r listElem $$PWD/cnt-pair-r-r-listElem >> obs.txt 2>> obs.err || true
	echo "--- pair-r-r callback ---" >> obs.txt
	[ -x pair-r-r ] && ./pair-r-r callback $$PWD/cnt-pair-r-r-callback >> obs.txt 2>> obs.err || true
	echo "--- pair-r-r pinned ---" >> obs.txt
	[ -x pair-r-r ] && ./pair-r-r pinned $$PWD/cnt-pair-r-r-pinned >> obs.txt 2>> obs.err || true
	echo "--- pair-r-r relay ---" >> obs.txt
	[ -x pair-r-r ] && ./pair-r-r relay $$PWD/cnt-pair-r-r-relay >> obs.txt 2>> obs.err || true
	echo "--- pair-r-r closureResult ---" >> obs.txt
	[ -x pair-r-r ] && ./pair-r-r closureResult $$PWD/cnt-pair-r-r-closureResult >> obs.txt 2>> obs.err || true
	echo "--- pair-r-r nestedSusp ---" >> obs.txt
	[ -x pair-r-r ] && ./pair-r-r nestedSusp $$PWD/cnt-pair-r-r-nestedSusp >> obs.txt 2>> obs.err || true
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
	morloc make -o nested-r-rust nested-r-rust.loc 2>> build.err || echo "--- nested-r-rust --- BUILD FAILED" >> obs.txt
	echo "--- nested-r-rust closureResult ---" >> obs.txt
	[ -x nested-r-rust ] && ./nested-r-rust closureResult $$PWD/cnt-nested-r-rust-closureResult >> obs.txt 2>> obs.err || true
	echo "--- nested-r-rust nestedSusp ---" >> obs.txt
	[ -x nested-r-rust ] && ./nested-r-rust nestedSusp $$PWD/cnt-nested-r-rust-nestedSusp >> obs.txt 2>> obs.err || true
	morloc make -o pair-rust-py pair-rust-py.loc 2>> build.err || echo "--- pair-rust-py --- BUILD FAILED" >> obs.txt
	echo "--- pair-rust-py rootResult ---" >> obs.txt
	[ -x pair-rust-py ] && ./pair-rust-py rootResult $$PWD/cnt-pair-rust-py-rootResult >> obs.txt 2>> obs.err || true
	echo "--- pair-rust-py rootArg ---" >> obs.txt
	[ -x pair-rust-py ] && ./pair-rust-py rootArg $$PWD/cnt-pair-rust-py-rootArg >> obs.txt 2>> obs.err || true
	echo "--- pair-rust-py recordField ---" >> obs.txt
	[ -x pair-rust-py ] && ./pair-rust-py recordField $$PWD/cnt-pair-rust-py-recordField >> obs.txt 2>> obs.err || true
	echo "--- pair-rust-py listElem ---" >> obs.txt
	[ -x pair-rust-py ] && ./pair-rust-py listElem $$PWD/cnt-pair-rust-py-listElem >> obs.txt 2>> obs.err || true
	echo "--- pair-rust-py callback ---" >> obs.txt
	[ -x pair-rust-py ] && ./pair-rust-py callback $$PWD/cnt-pair-rust-py-callback >> obs.txt 2>> obs.err || true
	echo "--- pair-rust-py pinned ---" >> obs.txt
	[ -x pair-rust-py ] && ./pair-rust-py pinned $$PWD/cnt-pair-rust-py-pinned >> obs.txt 2>> obs.err || true
	echo "--- pair-rust-py relay ---" >> obs.txt
	[ -x pair-rust-py ] && ./pair-rust-py relay $$PWD/cnt-pair-rust-py-relay >> obs.txt 2>> obs.err || true
	morloc make -o nested-rust-py nested-rust-py.loc 2>> build.err || echo "--- nested-rust-py --- BUILD FAILED" >> obs.txt
	echo "--- nested-rust-py closureResult ---" >> obs.txt
	[ -x nested-rust-py ] && ./nested-rust-py closureResult $$PWD/cnt-nested-rust-py-closureResult >> obs.txt 2>> obs.err || true
	echo "--- nested-rust-py nestedSusp ---" >> obs.txt
	[ -x nested-rust-py ] && ./nested-rust-py nestedSusp $$PWD/cnt-nested-rust-py-nestedSusp >> obs.txt 2>> obs.err || true
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
	morloc make -o nested-rust-cpp nested-rust-cpp.loc 2>> build.err || echo "--- nested-rust-cpp --- BUILD FAILED" >> obs.txt
	echo "--- nested-rust-cpp closureResult ---" >> obs.txt
	[ -x nested-rust-cpp ] && ./nested-rust-cpp closureResult $$PWD/cnt-nested-rust-cpp-closureResult >> obs.txt 2>> obs.err || true
	echo "--- nested-rust-cpp nestedSusp ---" >> obs.txt
	[ -x nested-rust-cpp ] && ./nested-rust-cpp nestedSusp $$PWD/cnt-nested-rust-cpp-nestedSusp >> obs.txt 2>> obs.err || true
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
	morloc make -o nested-rust-r nested-rust-r.loc 2>> build.err || echo "--- nested-rust-r --- BUILD FAILED" >> obs.txt
	echo "--- nested-rust-r closureResult ---" >> obs.txt
	[ -x nested-rust-r ] && ./nested-rust-r closureResult $$PWD/cnt-nested-rust-r-closureResult >> obs.txt 2>> obs.err || true
	echo "--- nested-rust-r nestedSusp ---" >> obs.txt
	[ -x nested-rust-r ] && ./nested-rust-r nestedSusp $$PWD/cnt-nested-rust-r-nestedSusp >> obs.txt 2>> obs.err || true
	morloc make -o pair-rust-rust pair-rust-rust.loc 2>> build.err || echo "--- pair-rust-rust --- BUILD FAILED" >> obs.txt
	echo "--- pair-rust-rust rootResult ---" >> obs.txt
	[ -x pair-rust-rust ] && ./pair-rust-rust rootResult $$PWD/cnt-pair-rust-rust-rootResult >> obs.txt 2>> obs.err || true
	echo "--- pair-rust-rust rootArg ---" >> obs.txt
	[ -x pair-rust-rust ] && ./pair-rust-rust rootArg $$PWD/cnt-pair-rust-rust-rootArg >> obs.txt 2>> obs.err || true
	echo "--- pair-rust-rust recordField ---" >> obs.txt
	[ -x pair-rust-rust ] && ./pair-rust-rust recordField $$PWD/cnt-pair-rust-rust-recordField >> obs.txt 2>> obs.err || true
	echo "--- pair-rust-rust listElem ---" >> obs.txt
	[ -x pair-rust-rust ] && ./pair-rust-rust listElem $$PWD/cnt-pair-rust-rust-listElem >> obs.txt 2>> obs.err || true
	echo "--- pair-rust-rust callback ---" >> obs.txt
	[ -x pair-rust-rust ] && ./pair-rust-rust callback $$PWD/cnt-pair-rust-rust-callback >> obs.txt 2>> obs.err || true
	echo "--- pair-rust-rust pinned ---" >> obs.txt
	[ -x pair-rust-rust ] && ./pair-rust-rust pinned $$PWD/cnt-pair-rust-rust-pinned >> obs.txt 2>> obs.err || true
	echo "--- pair-rust-rust relay ---" >> obs.txt
	[ -x pair-rust-rust ] && ./pair-rust-rust relay $$PWD/cnt-pair-rust-rust-relay >> obs.txt 2>> obs.err || true
	morloc make -o nested-rust-rust nested-rust-rust.loc 2>> build.err || echo "--- nested-rust-rust --- BUILD FAILED" >> obs.txt
	echo "--- nested-rust-rust closureResult ---" >> obs.txt
	[ -x nested-rust-rust ] && ./nested-rust-rust closureResult $$PWD/cnt-nested-rust-rust-closureResult >> obs.txt 2>> obs.err || true
	echo "--- nested-rust-rust nestedSusp ---" >> obs.txt
	[ -x nested-rust-rust ] && ./nested-rust-rust nestedSusp $$PWD/cnt-nested-rust-rust-nestedSusp >> obs.txt 2>> obs.err || true

clean:
	rm -rf $(PROGRAMS) __pycache__ *.err cnt-*
	rm -rf *-build *build.tmp*
