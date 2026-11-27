all:
	: > obs.txt
	rm -f *.err cnt-*
	morloc make -o main-py main-py.loc 2>> build.err || echo "--- main-py --- BUILD FAILED" >> obs.txt
	echo "--- main-py recHelper ---" >> obs.txt
	[ -x main-py ] && ./main-py recHelper $$PWD/cnt-main-py-recHelper >> obs.txt 2>> obs.err || true
	echo "--- main-py recLoop ---" >> obs.txt
	[ -x main-py ] && ./main-py recLoop $$PWD/cnt-main-py-recLoop >> obs.txt 2>> obs.err || true
	echo "--- main-py getter ---" >> obs.txt
	[ -x main-py ] && ./main-py getter $$PWD/cnt-main-py-getter >> obs.txt 2>> obs.err || true
	echo "--- main-py nested ---" >> obs.txt
	[ -x main-py ] && ./main-py nested $$PWD/cnt-main-py-nested >> obs.txt 2>> obs.err || true
	echo "--- main-py tupHelper ---" >> obs.txt
	[ -x main-py ] && ./main-py tupHelper $$PWD/cnt-main-py-tupHelper >> obs.txt 2>> obs.err || true
	echo "--- main-py listHelper ---" >> obs.txt
	[ -x main-py ] && ./main-py listHelper $$PWD/cnt-main-py-listHelper >> obs.txt 2>> obs.err || true
	echo "--- main-py optLiteral ---" >> obs.txt
	[ -x main-py ] && ./main-py optLiteral $$PWD/cnt-main-py-optLiteral >> obs.txt 2>> obs.err || true
	echo "--- main-py optHelper ---" >> obs.txt
	[ -x main-py ] && ./main-py optHelper $$PWD/cnt-main-py-optHelper >> obs.txt 2>> obs.err || true
	echo "--- main-py mapHelper ---" >> obs.txt
	[ -x main-py ] && ./main-py mapHelper $$PWD/cnt-main-py-mapHelper >> obs.txt 2>> obs.err || true
	echo "--- main-py suspOps ---" >> obs.txt
	[ -x main-py ] && ./main-py suspOps $$PWD/cnt-main-py-suspOps >> obs.txt 2>> obs.err || true
	echo "--- main-py hostRec ---" >> obs.txt
	[ -x main-py ] && ./main-py hostRec $$PWD/cnt-main-py-hostRec >> obs.txt 2>> obs.err || true
	echo "--- main-py listRoundTrip ---" >> obs.txt
	[ -x main-py ] && ./main-py listRoundTrip $$PWD/cnt-main-py-listRoundTrip >> obs.txt 2>> obs.err || true
	echo "--- main-py roundTrip ---" >> obs.txt
	[ -x main-py ] && ./main-py roundTrip $$PWD/cnt-main-py-roundTrip >> obs.txt 2>> obs.err || true
	echo "--- main-py aliased ---" >> obs.txt
	[ -x main-py ] && ./main-py aliased $$PWD/cnt-main-py-aliased >> obs.txt 2>> obs.err || true
	echo "--- main-py cbRet ---" >> obs.txt
	[ -x main-py ] && ./main-py cbRet $$PWD/cnt-main-py-cbRet >> obs.txt 2>> obs.err || true
	echo "--- main-py cbParam ---" >> obs.txt
	[ -x main-py ] && ./main-py cbParam $$PWD/cnt-main-py-cbParam >> obs.txt 2>> obs.err || true
	morloc make -o main-cpp main-cpp.loc 2>> build.err || echo "--- main-cpp --- BUILD FAILED" >> obs.txt
	echo "--- main-cpp recHelper ---" >> obs.txt
	[ -x main-cpp ] && ./main-cpp recHelper $$PWD/cnt-main-cpp-recHelper >> obs.txt 2>> obs.err || true
	echo "--- main-cpp recLoop ---" >> obs.txt
	[ -x main-cpp ] && ./main-cpp recLoop $$PWD/cnt-main-cpp-recLoop >> obs.txt 2>> obs.err || true
	echo "--- main-cpp getter ---" >> obs.txt
	[ -x main-cpp ] && ./main-cpp getter $$PWD/cnt-main-cpp-getter >> obs.txt 2>> obs.err || true
	echo "--- main-cpp nested ---" >> obs.txt
	[ -x main-cpp ] && ./main-cpp nested $$PWD/cnt-main-cpp-nested >> obs.txt 2>> obs.err || true
	echo "--- main-cpp tupHelper ---" >> obs.txt
	[ -x main-cpp ] && ./main-cpp tupHelper $$PWD/cnt-main-cpp-tupHelper >> obs.txt 2>> obs.err || true
	echo "--- main-cpp listHelper ---" >> obs.txt
	[ -x main-cpp ] && ./main-cpp listHelper $$PWD/cnt-main-cpp-listHelper >> obs.txt 2>> obs.err || true
	echo "--- main-cpp optLiteral ---" >> obs.txt
	[ -x main-cpp ] && ./main-cpp optLiteral $$PWD/cnt-main-cpp-optLiteral >> obs.txt 2>> obs.err || true
	echo "--- main-cpp optHelper ---" >> obs.txt
	[ -x main-cpp ] && ./main-cpp optHelper $$PWD/cnt-main-cpp-optHelper >> obs.txt 2>> obs.err || true
	echo "--- main-cpp mapHelper ---" >> obs.txt
	[ -x main-cpp ] && ./main-cpp mapHelper $$PWD/cnt-main-cpp-mapHelper >> obs.txt 2>> obs.err || true
	echo "--- main-cpp suspOps ---" >> obs.txt
	[ -x main-cpp ] && ./main-cpp suspOps $$PWD/cnt-main-cpp-suspOps >> obs.txt 2>> obs.err || true
	echo "--- main-cpp hostRec ---" >> obs.txt
	[ -x main-cpp ] && ./main-cpp hostRec $$PWD/cnt-main-cpp-hostRec >> obs.txt 2>> obs.err || true
	echo "--- main-cpp listRoundTrip ---" >> obs.txt
	[ -x main-cpp ] && ./main-cpp listRoundTrip $$PWD/cnt-main-cpp-listRoundTrip >> obs.txt 2>> obs.err || true
	echo "--- main-cpp roundTrip ---" >> obs.txt
	[ -x main-cpp ] && ./main-cpp roundTrip $$PWD/cnt-main-cpp-roundTrip >> obs.txt 2>> obs.err || true
	echo "--- main-cpp aliased ---" >> obs.txt
	[ -x main-cpp ] && ./main-cpp aliased $$PWD/cnt-main-cpp-aliased >> obs.txt 2>> obs.err || true
	echo "--- main-cpp cbRet ---" >> obs.txt
	[ -x main-cpp ] && ./main-cpp cbRet $$PWD/cnt-main-cpp-cbRet >> obs.txt 2>> obs.err || true
	echo "--- main-cpp cbParam ---" >> obs.txt
	[ -x main-cpp ] && ./main-cpp cbParam $$PWD/cnt-main-cpp-cbParam >> obs.txt 2>> obs.err || true
	morloc make -o main-rust main-rust.loc 2>> build.err || echo "--- main-rust --- BUILD FAILED" >> obs.txt
	echo "--- main-rust recHelper ---" >> obs.txt
	[ -x main-rust ] && ./main-rust recHelper $$PWD/cnt-main-rust-recHelper >> obs.txt 2>> obs.err || true
	echo "--- main-rust recLoop ---" >> obs.txt
	[ -x main-rust ] && ./main-rust recLoop $$PWD/cnt-main-rust-recLoop >> obs.txt 2>> obs.err || true
	echo "--- main-rust getter ---" >> obs.txt
	[ -x main-rust ] && ./main-rust getter $$PWD/cnt-main-rust-getter >> obs.txt 2>> obs.err || true
	echo "--- main-rust nested ---" >> obs.txt
	[ -x main-rust ] && ./main-rust nested $$PWD/cnt-main-rust-nested >> obs.txt 2>> obs.err || true
	echo "--- main-rust tupHelper ---" >> obs.txt
	[ -x main-rust ] && ./main-rust tupHelper $$PWD/cnt-main-rust-tupHelper >> obs.txt 2>> obs.err || true
	echo "--- main-rust listHelper ---" >> obs.txt
	[ -x main-rust ] && ./main-rust listHelper $$PWD/cnt-main-rust-listHelper >> obs.txt 2>> obs.err || true
	echo "--- main-rust optLiteral ---" >> obs.txt
	[ -x main-rust ] && ./main-rust optLiteral $$PWD/cnt-main-rust-optLiteral >> obs.txt 2>> obs.err || true
	echo "--- main-rust optHelper ---" >> obs.txt
	[ -x main-rust ] && ./main-rust optHelper $$PWD/cnt-main-rust-optHelper >> obs.txt 2>> obs.err || true
	echo "--- main-rust suspOps ---" >> obs.txt
	[ -x main-rust ] && ./main-rust suspOps $$PWD/cnt-main-rust-suspOps >> obs.txt 2>> obs.err || true
	echo "--- main-rust hostRec ---" >> obs.txt
	[ -x main-rust ] && ./main-rust hostRec $$PWD/cnt-main-rust-hostRec >> obs.txt 2>> obs.err || true
	echo "--- main-rust listRoundTrip ---" >> obs.txt
	[ -x main-rust ] && ./main-rust listRoundTrip $$PWD/cnt-main-rust-listRoundTrip >> obs.txt 2>> obs.err || true
	echo "--- main-rust roundTrip ---" >> obs.txt
	[ -x main-rust ] && ./main-rust roundTrip $$PWD/cnt-main-rust-roundTrip >> obs.txt 2>> obs.err || true
	echo "--- main-rust aliased ---" >> obs.txt
	[ -x main-rust ] && ./main-rust aliased $$PWD/cnt-main-rust-aliased >> obs.txt 2>> obs.err || true
	echo "--- main-rust cbRet ---" >> obs.txt
	[ -x main-rust ] && ./main-rust cbRet $$PWD/cnt-main-rust-cbRet >> obs.txt 2>> obs.err || true
	echo "--- main-rust cbParam ---" >> obs.txt
	[ -x main-rust ] && ./main-rust cbParam $$PWD/cnt-main-rust-cbParam >> obs.txt 2>> obs.err || true
	morloc make -o main-r main-r.loc 2>> build.err || echo "--- main-r --- BUILD FAILED" >> obs.txt
	echo "--- main-r recHelper ---" >> obs.txt
	[ -x main-r ] && ./main-r recHelper $$PWD/cnt-main-r-recHelper >> obs.txt 2>> obs.err || true
	echo "--- main-r recLoop ---" >> obs.txt
	[ -x main-r ] && ./main-r recLoop $$PWD/cnt-main-r-recLoop >> obs.txt 2>> obs.err || true
	echo "--- main-r getter ---" >> obs.txt
	[ -x main-r ] && ./main-r getter $$PWD/cnt-main-r-getter >> obs.txt 2>> obs.err || true
	echo "--- main-r nested ---" >> obs.txt
	[ -x main-r ] && ./main-r nested $$PWD/cnt-main-r-nested >> obs.txt 2>> obs.err || true
	echo "--- main-r tupHelper ---" >> obs.txt
	[ -x main-r ] && ./main-r tupHelper $$PWD/cnt-main-r-tupHelper >> obs.txt 2>> obs.err || true
	echo "--- main-r listHelper ---" >> obs.txt
	[ -x main-r ] && ./main-r listHelper $$PWD/cnt-main-r-listHelper >> obs.txt 2>> obs.err || true
	echo "--- main-r optLiteral ---" >> obs.txt
	[ -x main-r ] && ./main-r optLiteral $$PWD/cnt-main-r-optLiteral >> obs.txt 2>> obs.err || true
	echo "--- main-r optHelper ---" >> obs.txt
	[ -x main-r ] && ./main-r optHelper $$PWD/cnt-main-r-optHelper >> obs.txt 2>> obs.err || true
	echo "--- main-r mapHelper ---" >> obs.txt
	[ -x main-r ] && ./main-r mapHelper $$PWD/cnt-main-r-mapHelper >> obs.txt 2>> obs.err || true
	echo "--- main-r suspOps ---" >> obs.txt
	[ -x main-r ] && ./main-r suspOps $$PWD/cnt-main-r-suspOps >> obs.txt 2>> obs.err || true
	echo "--- main-r hostRec ---" >> obs.txt
	[ -x main-r ] && ./main-r hostRec $$PWD/cnt-main-r-hostRec >> obs.txt 2>> obs.err || true
	echo "--- main-r listRoundTrip ---" >> obs.txt
	[ -x main-r ] && ./main-r listRoundTrip $$PWD/cnt-main-r-listRoundTrip >> obs.txt 2>> obs.err || true
	echo "--- main-r roundTrip ---" >> obs.txt
	[ -x main-r ] && ./main-r roundTrip $$PWD/cnt-main-r-roundTrip >> obs.txt 2>> obs.err || true
	echo "--- main-r aliased ---" >> obs.txt
	[ -x main-r ] && ./main-r aliased $$PWD/cnt-main-r-aliased >> obs.txt 2>> obs.err || true
	echo "--- main-r cbRet ---" >> obs.txt
	[ -x main-r ] && ./main-r cbRet $$PWD/cnt-main-r-cbRet >> obs.txt 2>> obs.err || true
	echo "--- main-r cbParam ---" >> obs.txt
	[ -x main-r ] && ./main-r cbParam $$PWD/cnt-main-r-cbParam >> obs.txt 2>> obs.err || true
	morloc make -o main-mixed main-mixed.loc 2>> build.err || echo "--- main-mixed --- BUILD FAILED" >> obs.txt
	echo "--- main-mixed pyToRust ---" >> obs.txt
	[ -x main-mixed ] && ./main-mixed pyToRust $$PWD/cnt-main-mixed-pyToRust >> obs.txt 2>> obs.err || true
	echo "--- main-mixed rustToPy ---" >> obs.txt
	[ -x main-mixed ] && ./main-mixed rustToPy $$PWD/cnt-main-mixed-rustToPy >> obs.txt 2>> obs.err || true
	echo "--- main-mixed sharedPyToPy ---" >> obs.txt
	[ -x main-mixed ] && ./main-mixed sharedPyToPy $$PWD/cnt-main-mixed-sharedPyToPy >> obs.txt 2>> obs.err || true
	echo "--- main-mixed sharedPyToRust ---" >> obs.txt
	[ -x main-mixed ] && ./main-mixed sharedPyToRust $$PWD/cnt-main-mixed-sharedPyToRust >> obs.txt 2>> obs.err || true
	echo "--- main-mixed sharedRsToRust ---" >> obs.txt
	[ -x main-mixed ] && ./main-mixed sharedRsToRust $$PWD/cnt-main-mixed-sharedRsToRust >> obs.txt 2>> obs.err || true
	echo "--- main-mixed sharedRsToPy ---" >> obs.txt
	[ -x main-mixed ] && ./main-mixed sharedRsToPy $$PWD/cnt-main-mixed-sharedRsToPy >> obs.txt 2>> obs.err || true
	echo "--- main-mixed pyArgToRust ---" >> obs.txt
	[ -x main-mixed ] && ./main-mixed pyArgToRust $$PWD/cnt-main-mixed-pyArgToRust >> obs.txt 2>> obs.err || true
	echo "--- main-mixed rustArgToPy ---" >> obs.txt
	[ -x main-mixed ] && ./main-mixed rustArgToPy $$PWD/cnt-main-mixed-rustArgToPy >> obs.txt 2>> obs.err || true
	morloc make -o main-recursive main-recursive.loc 2>> build.err || echo "--- main-recursive --- BUILD FAILED" >> obs.txt
	echo "--- main-recursive chainDepth ---" >> obs.txt
	[ -x main-recursive ] && ./main-recursive chainDepth $$PWD/cnt-main-recursive-chainDepth >> obs.txt 2>> obs.err || true
	if morloc make -o main-refuse main-refuse.loc 2>> build.err; then echo "--- main-refuse --- UNEXPECTED" >> obs.txt; else echo "--- main-refuse --- OK" >> obs.txt; fi

clean:
	rm -rf main-py main-cpp main-rust main-r main-mixed main-recursive main-refuse __pycache__ *.err cnt-*
	rm -rf *-build *build.tmp*
