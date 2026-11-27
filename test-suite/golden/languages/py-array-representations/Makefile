all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus int64ForReal 3 >> obs.txt 2>> obs.err
	./nexus float32ForReal 3 >> obs.txt 2>> obs.err
	./nexus uint8ForEnum 3 >> obs.txt 2>> obs.err
	if ./nexus badUint8ForEnum 2 > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus matrixForPairs 2 > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus bytesForInt32 1 > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	./nexus bytesForUInt8 1 >> obs.txt 2>> obs.err
	if ./nexus badBytesForBool 1 > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	./nexus stridedForReal 1 >> obs.txt 2>> obs.err
	./nexus swappedForReal 1 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err __pycache__
	rm -rf *-build *build.tmp*
