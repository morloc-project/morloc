all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- indexPos ---" > obs.txt
	./nexus indexPos >> obs.txt 2>> obs.err
	echo "--- sliceFull ---" >> obs.txt
	./nexus sliceFull >> obs.txt 2>> obs.err
	echo "--- sliceFullStep ---" >> obs.txt
	./nexus sliceFullStep >> obs.txt 2>> obs.err
	echo "--- sliceOpenLeft ---" >> obs.txt
	./nexus sliceOpenLeft >> obs.txt 2>> obs.err
	echo "--- sliceOpenRight ---" >> obs.txt
	./nexus sliceOpenRight >> obs.txt 2>> obs.err
	echo "--- sliceClosed ---" >> obs.txt
	./nexus sliceClosed >> obs.txt 2>> obs.err
	echo "--- sliceStep ---" >> obs.txt
	./nexus sliceStep >> obs.txt 2>> obs.err
	echo "--- sliceStepClosed ---" >> obs.txt
	./nexus sliceStepClosed >> obs.txt 2>> obs.err
	echo "--- sliceEmpty ---" >> obs.txt
	./nexus sliceEmpty >> obs.txt 2>> obs.err
	echo "--- indexAnnotInt8 ---" >> obs.txt
	./nexus indexAnnotInt8 >> obs.txt 2>> obs.err
	echo "--- indexAnnotUInt32 ---" >> obs.txt
	./nexus indexAnnotUInt32 >> obs.txt 2>> obs.err
	echo "--- sliceAnnotBound ---" >> obs.txt
	./nexus sliceAnnotBound >> obs.txt 2>> obs.err
	echo "--- sliceAnnotNull ---" >> obs.txt
	./nexus sliceAnnotNull >> obs.txt 2>> obs.err
	echo "--- chainSliceField ---" >> obs.txt
	./nexus chainSliceField >> obs.txt 2>> obs.err
	echo "--- chainSliceSlice ---" >> obs.txt
	./nexus chainSliceSlice >> obs.txt 2>> obs.err
	echo "--- chainSliceSliceIdx ---" >> obs.txt
	./nexus chainSliceSliceIdx >> obs.txt 2>> obs.err
	echo "--- chainIndexField ---" >> obs.txt
	./nexus chainIndexField >> obs.txt 2>> obs.err
	echo "--- chainSliceFieldTuple ---" >> obs.txt
	./nexus chainSliceFieldTuple >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err __pycache__
