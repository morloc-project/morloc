all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- testRangeCpp ---" > obs.txt
	./nexus testRangeCpp >> obs.txt 2>> obs.err
	echo "--- testRangePy ---" >> obs.txt
	./nexus testRangePy >> obs.txt 2>> obs.err
	echo "--- testRangeR ---" >> obs.txt
	./nexus testRangeR >> obs.txt 2>> obs.err
	echo "--- testRecordCpp ---" >> obs.txt
	./nexus testRecordCpp >> obs.txt 2>> obs.err
	echo "--- testRecordPy ---" >> obs.txt
	./nexus testRecordPy >> obs.txt 2>> obs.err
	echo "--- testRecordR ---" >> obs.txt
	./nexus testRecordR >> obs.txt 2>> obs.err
	echo "--- indexOne ---" >> obs.txt
	./nexus indexOne >> obs.txt 2>> obs.err
	echo "--- sliceClosed ---" >> obs.txt
	./nexus sliceClosed >> obs.txt 2>> obs.err
	echo "--- sliceOpenLeft ---" >> obs.txt
	./nexus sliceOpenLeft >> obs.txt 2>> obs.err
	echo "--- sliceOpenRight ---" >> obs.txt
	./nexus sliceOpenRight >> obs.txt 2>> obs.err
	echo "--- sliceFull ---" >> obs.txt
	./nexus sliceFull >> obs.txt 2>> obs.err
	echo "--- sliceReverse ---" >> obs.txt
	./nexus sliceReverse >> obs.txt 2>> obs.err
	echo "--- sliceEvery ---" >> obs.txt
	./nexus sliceEvery >> obs.txt 2>> obs.err
	echo "--- sliceNegEnd ---" >> obs.txt
	./nexus sliceNegEnd >> obs.txt 2>> obs.err
	echo "--- sliceWithStep ---" >> obs.txt
	./nexus sliceWithStep >> obs.txt 2>> obs.err
	echo "--- chainedFieldThenSlice ---" >> obs.txt
	./nexus chainedFieldThenSlice >> obs.txt 2>> obs.err
	echo "--- chainedIndexThenField ---" >> obs.txt
	./nexus chainedIndexThenField >> obs.txt 2>> obs.err
	echo "--- sliceThenTupleField ---" >> obs.txt
	./nexus sliceThenTupleField >> obs.txt 2>> obs.err
	echo "--- sliceThenRecordField ---" >> obs.txt
	./nexus sliceThenRecordField >> obs.txt 2>> obs.err
	echo "--- sliceThenNestedSlice ---" >> obs.txt
	./nexus sliceThenNestedSlice >> obs.txt 2>> obs.err
	echo "--- sliceThenGroup ---" >> obs.txt
	./nexus sliceThenGroup >> obs.txt 2>> obs.err
	echo "--- deepChain ---" >> obs.txt
	./nexus deepChain >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err __pycache__
