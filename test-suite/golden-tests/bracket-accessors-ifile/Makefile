all:
	rm -f *.err obs.txt /tmp/bai-*.pkt
	morloc make -o nexus main.loc 2> build.err
	echo "--- testIFileRangeCpp ---" > obs.txt
	./nexus testIFileRangeCpp                 "/tmp/bai-range-cpp.pkt"          >> obs.txt 2>> obs.err
	echo "--- testIFileRangePy ---"           >> obs.txt
	./nexus testIFileRangePy                  "/tmp/bai-range-py.pkt"           >> obs.txt 2>> obs.err
	echo "--- testIFileRangeR ---"            >> obs.txt
	./nexus testIFileRangeR                   "/tmp/bai-range-r.pkt"            >> obs.txt 2>> obs.err
	echo "--- testIFileRecordCpp ---"         >> obs.txt
	./nexus testIFileRecordCpp                "/tmp/bai-rec-cpp-pts.pkt"        "/tmp/bai-rec-cpp-pairs.pkt"  "/tmp/bai-rec-cpp-rows.pkt"  >> obs.txt 2>> obs.err
	echo "--- testIFileRecordPy ---"          >> obs.txt
	./nexus testIFileRecordPy                 "/tmp/bai-rec-py-pts.pkt"         "/tmp/bai-rec-py-pairs.pkt"   "/tmp/bai-rec-py-rows.pkt"   >> obs.txt 2>> obs.err
	echo "--- testIFileRecordR ---"           >> obs.txt
	./nexus testIFileRecordR                  "/tmp/bai-rec-r-pts.pkt"          "/tmp/bai-rec-r-pairs.pkt"    "/tmp/bai-rec-r-rows.pkt"    >> obs.txt 2>> obs.err
	echo "--- ifileIndexOne ---"              >> obs.txt
	./nexus ifileIndexOne                     "/tmp/bai-idx-one.pkt"            >> obs.txt 2>> obs.err
	echo "--- ifileSliceClosed ---"           >> obs.txt
	./nexus ifileSliceClosed                  "/tmp/bai-slc-closed.pkt"         >> obs.txt 2>> obs.err
	echo "--- ifileSliceOpenLeft ---"         >> obs.txt
	./nexus ifileSliceOpenLeft                "/tmp/bai-slc-openl.pkt"          >> obs.txt 2>> obs.err
	echo "--- ifileSliceOpenRight ---"        >> obs.txt
	./nexus ifileSliceOpenRight               "/tmp/bai-slc-openr.pkt"          >> obs.txt 2>> obs.err
	echo "--- ifileSliceFull ---"             >> obs.txt
	./nexus ifileSliceFull                    "/tmp/bai-slc-full.pkt"           >> obs.txt 2>> obs.err
	echo "--- ifileSliceReverse ---"          >> obs.txt
	./nexus ifileSliceReverse                 "/tmp/bai-slc-rev.pkt"            >> obs.txt 2>> obs.err
	echo "--- ifileSliceEvery ---"            >> obs.txt
	./nexus ifileSliceEvery                   "/tmp/bai-slc-every.pkt"          >> obs.txt 2>> obs.err
	echo "--- ifileSliceNegEnd ---"           >> obs.txt
	./nexus ifileSliceNegEnd                  "/tmp/bai-slc-negend.pkt"         >> obs.txt 2>> obs.err
	echo "--- ifileSliceWithStep ---"         >> obs.txt
	./nexus ifileSliceWithStep                "/tmp/bai-slc-step.pkt"           >> obs.txt 2>> obs.err
	echo "--- ifileChainedFieldThenSlice ---" >> obs.txt
	./nexus ifileChainedFieldThenSlice        "/tmp/bai-chain-fts.pkt"          >> obs.txt 2>> obs.err
	echo "--- ifileChainedIndexThenField ---" >> obs.txt
	./nexus ifileChainedIndexThenField        "/tmp/bai-chain-itf.pkt"          >> obs.txt 2>> obs.err
	echo "--- ifileSliceThenTupleField ---"   >> obs.txt
	./nexus ifileSliceThenTupleField          "/tmp/bai-slc-then-tf.pkt"        >> obs.txt 2>> obs.err
	echo "--- ifileSliceThenRecordField ---"  >> obs.txt
	./nexus ifileSliceThenRecordField         "/tmp/bai-slc-then-rf.pkt"        >> obs.txt 2>> obs.err
	echo "--- ifileSliceThenNestedSlice ---"  >> obs.txt
	./nexus ifileSliceThenNestedSlice         "/tmp/bai-slc-then-ns.pkt"        >> obs.txt 2>> obs.err
	echo "--- ifileSliceThenGroup ---"        >> obs.txt
	./nexus ifileSliceThenGroup               "/tmp/bai-slc-then-grp.pkt"       >> obs.txt 2>> obs.err
	echo "--- ifileDeepChain ---"             >> obs.txt
	./nexus ifileDeepChain                    "/tmp/bai-deep.pkt"               >> obs.txt 2>> obs.err
	echo "--- ifileIndexFromForeignLength ---" >> obs.txt
	./nexus ifileIndexFromForeignLength       "/tmp/bai-idx-foreign.pkt"        >> obs.txt 2>> obs.err
	echo "--- ifileSliceFromOptionalStart ---" >> obs.txt
	./nexus ifileSliceFromOptionalStart       "/tmp/bai-slc-optstart.pkt"       >> obs.txt 2>> obs.err
	echo "--- ifileIndexI8 ---"             >> obs.txt
	./nexus ifileIndexI8                    "/tmp/bai-idx-i8.pkt"             >> obs.txt 2>> obs.err
	echo "--- ifileIndexI16 ---"            >> obs.txt
	./nexus ifileIndexI16                   "/tmp/bai-idx-i16.pkt"            >> obs.txt 2>> obs.err
	echo "--- ifileIndexU32 ---"           >> obs.txt
	./nexus ifileIndexU32                  "/tmp/bai-idx-u32.pkt"            >> obs.txt 2>> obs.err
	echo "--- ifileSliceI8Bound ---"        >> obs.txt
	./nexus ifileSliceI8Bound               "/tmp/bai-slc-i8.pkt"             >> obs.txt 2>> obs.err
	echo "--- ifileSliceMixedWidthBounds ---" >> obs.txt
	./nexus ifileSliceMixedWidthBounds        "/tmp/bai-slc-mixed.pkt"          >> obs.txt 2>> obs.err
	echo "--- ifileCrossPoolReceiverPyToCpp ---" >> obs.txt
	./nexus ifileCrossPoolReceiverPyToCpp     "/tmp/bai-xp-py2cpp.pkt"          >> obs.txt 2>> obs.err
	echo "--- ifileCrossPoolReceiverCppToR ---" >> obs.txt
	./nexus ifileCrossPoolReceiverCppToR      "/tmp/bai-xp-cpp2r.pkt"           >> obs.txt 2>> obs.err
	echo "--- ifileCrossPoolReceiverRToPy ---" >> obs.txt
	./nexus ifileCrossPoolReceiverRToPy       "/tmp/bai-xp-r2py.pkt"            >> obs.txt 2>> obs.err
	echo "--- ifileCrossPoolIdxForeign ---"   >> obs.txt
	./nexus ifileCrossPoolIdxForeign          "/tmp/bai-xp-idx-foreign.pkt"     >> obs.txt 2>> obs.err
	echo "--- ifileCrossPoolSliceNested ---"  >> obs.txt
	./nexus ifileCrossPoolSliceNested         "/tmp/bai-xp-slc-nested.pkt"      >> obs.txt 2>> obs.err
	rm -f /tmp/bai-*.pkt

clean:
	rm -rf nexus pools *err __pycache__
	rm -f /tmp/bai-*.pkt
