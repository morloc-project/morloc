all:
	rm -f *.err obs.txt
	rm -f /tmp/vector-comprehensive-*.dat
	morloc make -o nexus main.loc 2> build.err
	echo "=== (1) Round-trip Real across all six language pairs ===" > obs.txt
	echo "--- cpRound ---" >> obs.txt
	./nexus cpRound >> obs.txt 2>> obs.err
	echo "--- pcRound ---" >> obs.txt
	./nexus pcRound >> obs.txt 2>> obs.err
	echo "--- prRound ---" >> obs.txt
	./nexus prRound >> obs.txt 2>> obs.err
	echo "--- rpRound ---" >> obs.txt
	./nexus rpRound >> obs.txt 2>> obs.err
	echo "--- crRound ---" >> obs.txt
	./nexus crRound >> obs.txt 2>> obs.err
	echo "--- rcRound ---" >> obs.txt
	./nexus rcRound >> obs.txt 2>> obs.err
	echo "=== (2) Element types: Cpp <-> Py ===" >> obs.txt
	echo "--- cpI8 ---" >> obs.txt
	./nexus cpI8 >> obs.txt 2>> obs.err
	echo "--- pcI8 ---" >> obs.txt
	./nexus pcI8 >> obs.txt 2>> obs.err
	echo "--- cpI16 ---" >> obs.txt
	./nexus cpI16 >> obs.txt 2>> obs.err
	echo "--- pcI16 ---" >> obs.txt
	./nexus pcI16 >> obs.txt 2>> obs.err
	echo "--- cpI32 ---" >> obs.txt
	./nexus cpI32 >> obs.txt 2>> obs.err
	echo "--- pcI32 ---" >> obs.txt
	./nexus pcI32 >> obs.txt 2>> obs.err
	echo "--- cpI64 ---" >> obs.txt
	./nexus cpI64 >> obs.txt 2>> obs.err
	echo "--- pcI64 ---" >> obs.txt
	./nexus pcI64 >> obs.txt 2>> obs.err
	echo "--- cpU8 ---" >> obs.txt
	./nexus cpU8 >> obs.txt 2>> obs.err
	echo "--- pcU8 ---" >> obs.txt
	./nexus pcU8 >> obs.txt 2>> obs.err
	echo "--- cpU16 ---" >> obs.txt
	./nexus cpU16 >> obs.txt 2>> obs.err
	echo "--- pcU16 ---" >> obs.txt
	./nexus pcU16 >> obs.txt 2>> obs.err
	echo "--- cpU32 ---" >> obs.txt
	./nexus cpU32 >> obs.txt 2>> obs.err
	echo "--- pcU32 ---" >> obs.txt
	./nexus pcU32 >> obs.txt 2>> obs.err
	echo "--- cpU64 ---" >> obs.txt
	./nexus cpU64 >> obs.txt 2>> obs.err
	echo "--- pcU64 ---" >> obs.txt
	./nexus pcU64 >> obs.txt 2>> obs.err
	echo "--- cpF32 ---" >> obs.txt
	./nexus cpF32 >> obs.txt 2>> obs.err
	echo "--- pcF32 ---" >> obs.txt
	./nexus pcF32 >> obs.txt 2>> obs.err
	echo "--- cpF64 ---" >> obs.txt
	./nexus cpF64 >> obs.txt 2>> obs.err
	echo "--- pcF64 ---" >> obs.txt
	./nexus pcF64 >> obs.txt 2>> obs.err
	echo "=== (2b) Width-alias extremes ===" >> obs.txt
	echo "--- cpI8Min ---" >> obs.txt
	./nexus cpI8Min >> obs.txt 2>> obs.err
	echo "--- pcI8Min ---" >> obs.txt
	./nexus pcI8Min >> obs.txt 2>> obs.err
	echo "--- cpU8Max ---" >> obs.txt
	./nexus cpU8Max >> obs.txt 2>> obs.err
	echo "--- pcU8Max ---" >> obs.txt
	./nexus pcU8Max >> obs.txt 2>> obs.err
	echo "=== (3) Permutation probe: all six pairs (expect 8) ===" >> obs.txt
	echo "--- cpPerm ---" >> obs.txt
	./nexus cpPerm >> obs.txt 2>> obs.err
	echo "--- pcPerm ---" >> obs.txt
	./nexus pcPerm >> obs.txt 2>> obs.err
	echo "--- prPerm ---" >> obs.txt
	./nexus prPerm >> obs.txt 2>> obs.err
	echo "--- rpPerm ---" >> obs.txt
	./nexus rpPerm >> obs.txt 2>> obs.err
	echo "--- crPerm ---" >> obs.txt
	./nexus crPerm >> obs.txt 2>> obs.err
	echo "--- rcPerm ---" >> obs.txt
	./nexus rcPerm >> obs.txt 2>> obs.err
	echo "=== (4) File IO @save / @savej / @savem round-trip ===" >> obs.txt
	echo "--- fileBin ---" >> obs.txt
	./nexus --keep-null fileBin '"/tmp/vector-comprehensive-bin.dat"'  >> obs.txt 2>> obs.err
	echo "--- fileJson ---" >> obs.txt
	./nexus --keep-null fileJson '"/tmp/vector-comprehensive-json.dat"' >> obs.txt 2>> obs.err
	echo "--- fileMsg ---" >> obs.txt
	./nexus --keep-null fileMsg '"/tmp/vector-comprehensive-msg.dat"'  >> obs.txt 2>> obs.err
	echo "--- fileSumBin ---" >> obs.txt
	./nexus --keep-null fileSumBin '"/tmp/vector-comprehensive-sum-bin.dat"'  >> obs.txt 2>> obs.err
	echo "--- fileSumJson ---" >> obs.txt
	./nexus --keep-null fileSumJson '"/tmp/vector-comprehensive-sum-json.dat"' >> obs.txt 2>> obs.err
	echo "=== (5) Zero-copy direct assertions (numpy OWNDATA) ===" >> obs.txt
	echo "--- zcShmPyPy ---" >> obs.txt
	./nexus zcShmPyPy >> obs.txt 2>> obs.err
	echo "--- zcInlinePyPy ---" >> obs.txt
	./nexus zcInlinePyPy >> obs.txt 2>> obs.err
	echo "--- zcShmCpToPy ---" >> obs.txt
	./nexus zcShmCpToPy >> obs.txt 2>> obs.err
	echo "--- zcInlineCpToPy ---" >> obs.txt
	./nexus zcInlineCpToPy >> obs.txt 2>> obs.err
	echo "--- zcShmRToPy ---" >> obs.txt
	./nexus zcShmRToPy >> obs.txt 2>> obs.err
	echo "=== (6) Morloc-level operations on Vector ===" >> obs.txt
	echo "--- mlSum ---" >> obs.txt
	./nexus mlSum >> obs.txt 2>> obs.err
	echo "--- mlLength ---" >> obs.txt
	./nexus mlLength >> obs.txt 2>> obs.err
	echo "--- mlMap ---" >> obs.txt
	./nexus mlMap >> obs.txt 2>> obs.err
	echo "=== (7) Edge cases (empty / single) ===" >> obs.txt
	echo "--- emptyCross ---" >> obs.txt
	./nexus emptyCross >> obs.txt 2>> obs.err
	echo "--- singleCross ---" >> obs.txt
	./nexus singleCross >> obs.txt 2>> obs.err
	echo "=== (8) Threshold straddle (Cpp -> Py) ===" >> obs.txt
	echo "--- underThr ---" >> obs.txt
	./nexus underThr >> obs.txt 2>> obs.err
	echo "--- overThr ---" >> obs.txt
	./nexus overThr >> obs.txt 2>> obs.err
	echo "=== (9) Literal inputs: source literals + CLI literals ===" >> obs.txt
	echo "--- litSrcRealAnnot ---" >> obs.txt
	./nexus litSrcRealAnnot >> obs.txt 2>> obs.err
	echo "--- litSrcRealInfer ---" >> obs.txt
	./nexus litSrcRealInfer >> obs.txt 2>> obs.err
	echo "--- litSrcIntAnnot ---" >> obs.txt
	./nexus litSrcIntAnnot >> obs.txt 2>> obs.err
	echo "--- litCliReal ---" >> obs.txt
	./nexus litCliReal '[1.5,2.5,3.5,4.5,5.5]' >> obs.txt 2>> obs.err
	echo "--- litCliInt ---" >> obs.txt
	./nexus litCliInt  '[10,20,30,40,50]' >> obs.txt 2>> obs.err
	echo "--- litCliRealCpp ---" >> obs.txt
	./nexus litCliRealCpp '[1.5,2.5,3.5,4.5,5.5]' >> obs.txt 2>> obs.err
	echo "--- litCliIntCpp ---" >> obs.txt
	./nexus litCliIntCpp  '[10,20,30,40,50]' >> obs.txt 2>> obs.err
	echo "--- litCliRealR ---" >> obs.txt
	./nexus litCliRealR '[1.5,2.5,3.5,4.5,5.5]' >> obs.txt 2>> obs.err
	echo "--- litCliEmpty ---" >> obs.txt
	./nexus litCliEmpty '[]' >> obs.txt 2>> obs.err
	echo "--- litCliSingle ---" >> obs.txt
	./nexus litCliSingle '[42.0]' >> obs.txt 2>> obs.err
	rm -f /tmp/vector-comprehensive-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/vector-comprehensive-*.dat
