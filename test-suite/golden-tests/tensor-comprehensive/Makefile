all:
	rm -f *.err obs.txt
	rm -f /tmp/tensor-comprehensive-*.dat
	morloc make -o nexus main.loc 2> build.err
	echo "=== (1) Round-trip rank 2-5 across all six pairs ===" > obs.txt
	echo "--- cpMat ---" >> obs.txt
	./nexus cpMat >> obs.txt 2>> obs.err
	echo "--- pcMat ---" >> obs.txt
	./nexus pcMat >> obs.txt 2>> obs.err
	echo "--- prMat ---" >> obs.txt
	./nexus prMat >> obs.txt 2>> obs.err
	echo "--- rpMat ---" >> obs.txt
	./nexus rpMat >> obs.txt 2>> obs.err
	echo "--- crMat ---" >> obs.txt
	./nexus crMat >> obs.txt 2>> obs.err
	echo "--- rcMat ---" >> obs.txt
	./nexus rcMat >> obs.txt 2>> obs.err
	echo "--- cpT3 ---" >> obs.txt
	./nexus cpT3 >> obs.txt 2>> obs.err
	echo "--- pcT3 ---" >> obs.txt
	./nexus pcT3 >> obs.txt 2>> obs.err
	echo "--- prT3 ---" >> obs.txt
	./nexus prT3 >> obs.txt 2>> obs.err
	echo "--- rpT3 ---" >> obs.txt
	./nexus rpT3 >> obs.txt 2>> obs.err
	echo "--- crT3 ---" >> obs.txt
	./nexus crT3 >> obs.txt 2>> obs.err
	echo "--- rcT3 ---" >> obs.txt
	./nexus rcT3 >> obs.txt 2>> obs.err
	echo "--- cpT4 ---" >> obs.txt
	./nexus cpT4 >> obs.txt 2>> obs.err
	echo "--- pcT4 ---" >> obs.txt
	./nexus pcT4 >> obs.txt 2>> obs.err
	echo "--- prT4 ---" >> obs.txt
	./nexus prT4 >> obs.txt 2>> obs.err
	echo "--- rpT4 ---" >> obs.txt
	./nexus rpT4 >> obs.txt 2>> obs.err
	echo "--- crT4 ---" >> obs.txt
	./nexus crT4 >> obs.txt 2>> obs.err
	echo "--- rcT4 ---" >> obs.txt
	./nexus rcT4 >> obs.txt 2>> obs.err
	echo "--- cpT5 ---" >> obs.txt
	./nexus cpT5 >> obs.txt 2>> obs.err
	echo "--- pcT5 ---" >> obs.txt
	./nexus pcT5 >> obs.txt 2>> obs.err
	echo "--- prT5 ---" >> obs.txt
	./nexus prT5 >> obs.txt 2>> obs.err
	echo "--- rpT5 ---" >> obs.txt
	./nexus rpT5 >> obs.txt 2>> obs.err
	echo "--- crT5 ---" >> obs.txt
	./nexus crT5 >> obs.txt 2>> obs.err
	echo "--- rcT5 ---" >> obs.txt
	./nexus rcT5 >> obs.txt 2>> obs.err
	echo "=== (2) Stdlib operations ===" >> obs.txt
	echo "--- mmCpp ---" >> obs.txt
	./nexus mmCpp >> obs.txt 2>> obs.err
	echo "--- mmPy ---" >> obs.txt
	./nexus mmPy >> obs.txt 2>> obs.err
	echo "--- mmR ---" >> obs.txt
	./nexus mmR >> obs.txt 2>> obs.err
	echo "--- idSum3 ---" >> obs.txt
	./nexus idSum3 >> obs.txt 2>> obs.err
	echo "--- idSum5 ---" >> obs.txt
	./nexus idSum5 >> obs.txt 2>> obs.err
	echo "--- zerosSum ---" >> obs.txt
	./nexus zerosSum >> obs.txt 2>> obs.err
	echo "--- onesSum ---" >> obs.txt
	./nexus onesSum >> obs.txt 2>> obs.err
	echo "--- fillSum ---" >> obs.txt
	./nexus fillSum >> obs.txt 2>> obs.err
	echo "=== (3) Row/column-major corner extraction (expect [0,12,11,23]) ===" >> obs.txt
	echo "--- cornersCpToPy ---" >> obs.txt
	./nexus cornersCpToPy >> obs.txt 2>> obs.err
	echo "--- cornersCpToR ---" >> obs.txt
	./nexus cornersCpToR >> obs.txt 2>> obs.err
	echo "--- cornersRtoCp ---" >> obs.txt
	./nexus cornersRtoCp >> obs.txt 2>> obs.err
	echo "--- cornersPyToR ---" >> obs.txt
	./nexus cornersPyToR >> obs.txt 2>> obs.err
	echo "=== (4) File IO @save/@savej/@savem of a Matrix ===" >> obs.txt
	echo "--- matFileBin ---" >> obs.txt
	./nexus --keep-null matFileBin  '"/tmp/tensor-comprehensive-mat-bin.dat"'  >> obs.txt 2>> obs.err
	echo "--- matFileJson ---" >> obs.txt
	./nexus --keep-null matFileJson '"/tmp/tensor-comprehensive-mat-json.dat"' >> obs.txt 2>> obs.err
	echo "--- matFileMsg ---" >> obs.txt
	./nexus --keep-null matFileMsg  '"/tmp/tensor-comprehensive-mat-msg.dat"'  >> obs.txt 2>> obs.err
	echo "--- matFileSumBin ---" >> obs.txt
	./nexus --keep-null matFileSumBin  '"/tmp/tensor-comprehensive-mat-sum-bin.dat"'  >> obs.txt 2>> obs.err
	echo "--- matFileSumJson ---" >> obs.txt
	./nexus --keep-null matFileSumJson '"/tmp/tensor-comprehensive-mat-sum-json.dat"' >> obs.txt 2>> obs.err
	echo "=== (5) Matrix element types ===" >> obs.txt
	echo "--- matI32CpPy ---" >> obs.txt
	./nexus matI32CpPy >> obs.txt 2>> obs.err
	echo "--- matI32PyCp ---" >> obs.txt
	./nexus matI32PyCp >> obs.txt 2>> obs.err
	echo "--- matI64CpPy ---" >> obs.txt
	./nexus matI64CpPy >> obs.txt 2>> obs.err
	echo "--- matI64PyCp ---" >> obs.txt
	./nexus matI64PyCp >> obs.txt 2>> obs.err
	echo "--- matF32CpPy ---" >> obs.txt
	./nexus matF32CpPy >> obs.txt 2>> obs.err
	echo "--- matF32PyCp ---" >> obs.txt
	./nexus matF32PyCp >> obs.txt 2>> obs.err
	echo "=== (6) Zero-copy probes (DIRECT, ROOT, size) ===" >> obs.txt
	echo "--- zcMatShmCpToPy ---" >> obs.txt
	./nexus zcMatShmCpToPy >> obs.txt 2>> obs.err
	echo "--- zcMatInlineCpToPy ---" >> obs.txt
	./nexus zcMatInlineCpToPy >> obs.txt 2>> obs.err
	echo "--- zcMatShmRToPy ---" >> obs.txt
	./nexus zcMatShmRToPy >> obs.txt 2>> obs.err
	echo "--- zcT3ShmCpToPy ---" >> obs.txt
	./nexus zcT3ShmCpToPy >> obs.txt 2>> obs.err
	echo "--- zcT5ShmCpToPy ---" >> obs.txt
	./nexus zcT5ShmCpToPy >> obs.txt 2>> obs.err
	echo "=== (7) Edge cases: 1x1 matrix and 1x1x1 tensor ===" >> obs.txt
	echo "--- unitMatCross ---" >> obs.txt
	./nexus unitMatCross >> obs.txt 2>> obs.err
	echo "--- unitT3Cross ---" >> obs.txt
	./nexus unitT3Cross >> obs.txt 2>> obs.err
	echo "=== (8) Literal inputs: source literals + CLI literals ===" >> obs.txt
	echo "--- litSrcMatAnnot ---" >> obs.txt
	./nexus litSrcMatAnnot >> obs.txt 2>> obs.err
	echo "--- litSrcMatInfer ---" >> obs.txt
	./nexus litSrcMatInfer >> obs.txt 2>> obs.err
	echo "--- litSrcMatIntAnnot ---" >> obs.txt
	./nexus litSrcMatIntAnnot >> obs.txt 2>> obs.err
	echo "--- litSrcT3Annot ---" >> obs.txt
	./nexus litSrcT3Annot >> obs.txt 2>> obs.err
	echo "--- litSrcT3Infer ---" >> obs.txt
	./nexus litSrcT3Infer >> obs.txt 2>> obs.err
	echo "--- litCliMat ---" >> obs.txt
	./nexus litCliMat    '[[2,3],[1.5,2.5,3.5,4.5,5.5,6.5]]' >> obs.txt 2>> obs.err
	echo "--- litCliMatCpp ---" >> obs.txt
	./nexus litCliMatCpp '[[2,3],[1.5,2.5,3.5,4.5,5.5,6.5]]' >> obs.txt 2>> obs.err
	echo "--- litCliMatR ---" >> obs.txt
	./nexus litCliMatR   '[[2,3],[1.5,2.5,3.5,4.5,5.5,6.5]]' >> obs.txt 2>> obs.err
	echo "--- litCliMatInt ---" >> obs.txt
	./nexus litCliMatInt '[[2,3],[1,2,3,4,5,6]]' >> obs.txt 2>> obs.err
	echo "--- litCliT3 ---" >> obs.txt
	./nexus litCliT3     '[[2,2,2],[1,2,3,4,5,6,7,8]]' >> obs.txt 2>> obs.err
	rm -f /tmp/tensor-comprehensive-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/tensor-comprehensive-*.dat
