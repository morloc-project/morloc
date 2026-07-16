all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- vecReal4 ---" > obs.txt
	./nexus vecReal4 >> obs.txt 2>> obs.err
	echo "--- vecReal0 ---" >> obs.txt
	./nexus vecReal0 >> obs.txt 2>> obs.err
	echo "--- vecI3 ---" >> obs.txt
	./nexus vecI3 >> obs.txt 2>> obs.err
	echo "--- vecBool2 ---" >> obs.txt
	./nexus vecBool2 >> obs.txt 2>> obs.err
	echo "--- vecStr3 ---" >> obs.txt
	./nexus vecStr3 >> obs.txt 2>> obs.err
	echo "--- chainVecReal3 ---" >> obs.txt
	./nexus chainVecReal3 >> obs.txt 2>> obs.err
	echo "--- chainVecReal0 ---" >> obs.txt
	./nexus chainVecReal0 >> obs.txt 2>> obs.err
	echo "--- fixedPairInt ---" >> obs.txt
	./nexus fixedPairInt >> obs.txt 2>> obs.err
	echo "--- fixedPairStr ---" >> obs.txt
	./nexus fixedPairStr >> obs.txt 2>> obs.err
	echo "--- rtVecReal4 ---" >> obs.txt
	./nexus rtVecReal4 >> obs.txt 2>> obs.err
	echo "--- rtVecReal0 ---" >> obs.txt
	./nexus rtVecReal0 >> obs.txt 2>> obs.err
	echo "--- rtVecI3 ---" >> obs.txt
	./nexus rtVecI3 >> obs.txt 2>> obs.err
	echo "--- rtVecI0 ---" >> obs.txt
	./nexus rtVecI0 >> obs.txt 2>> obs.err
	echo "--- rtVecBool2 ---" >> obs.txt
	./nexus rtVecBool2 >> obs.txt 2>> obs.err
	echo "--- rtChainVec ---" >> obs.txt
	./nexus rtChainVec >> obs.txt 2>> obs.err
	echo "--- rtFixedPair ---" >> obs.txt
	./nexus rtFixedPair >> obs.txt 2>> obs.err
	echo "--- mixedFixedPair ---" >> obs.txt
	./nexus mixedFixedPair >> obs.txt 2>> obs.err
	echo "--- sumVecArg ---" >> obs.txt
	./nexus sumVecArg >> obs.txt 2>> obs.err
	echo "--- sumVecZeroArg ---" >> obs.txt
	./nexus sumVecZeroArg >> obs.txt 2>> obs.err
	echo "--- makeVecResult ---" >> obs.txt
	./nexus makeVecResult >> obs.txt 2>> obs.err
	echo "--- mat2x3 ---" >> obs.txt
	./nexus mat2x3 >> obs.txt 2>> obs.err
	echo "--- mat0x3 ---" >> obs.txt
	./nexus mat0x3 >> obs.txt 2>> obs.err
	echo "--- mat3x0 ---" >> obs.txt
	./nexus mat3x0 >> obs.txt 2>> obs.err
	echo "--- mat0x0 ---" >> obs.txt
	./nexus mat0x0 >> obs.txt 2>> obs.err
	echo "--- rtMat2x3 ---" >> obs.txt
	./nexus rtMat2x3 >> obs.txt 2>> obs.err
	echo "--- rtMat0x3 ---" >> obs.txt
	./nexus rtMat0x3 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
