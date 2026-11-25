all:
	rm -f *.err obs.txt
	rm -f /tmp/ifile-rec-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus flatKey         "/tmp/ifile-rec-flatkey.dat"    >  obs.txt 2>  obs.err
	./nexus keyGroup        "/tmp/ifile-rec-group.dat"      >> obs.txt 2>> obs.err
	./nexus keyGroupReversed "/tmp/ifile-rec-grouprev.dat"  >> obs.txt 2>> obs.err
	./nexus keyChain        "/tmp/ifile-rec-chain.dat"      >> obs.txt 2>> obs.err
	./nexus keyIndexKey     "/tmp/ifile-rec-keyidx.dat"     >> obs.txt 2>> obs.err
	./nexus chainGroup      "/tmp/ifile-rec-chaingrp.dat"   >> obs.txt 2>> obs.err
	./nexus repeatedKey     "/tmp/ifile-rec-repeat.dat"     >> obs.txt 2>> obs.err
	./nexus groupUnderKey   "/tmp/ifile-rec-underkey.dat"   >> obs.txt 2>> obs.err
	./nexus bracketThenKey  "/tmp/ifile-rec-brkkey.dat"     >> obs.txt 2>> obs.err
	./nexus sliceThenKey    "/tmp/ifile-rec-slicekey.dat"   >> obs.txt 2>> obs.err
	./nexus keyThenBracket  "/tmp/ifile-rec-keybrk.dat"     >> obs.txt 2>> obs.err
	./nexus keyThenSlice    "/tmp/ifile-rec-keyslice.dat"   >> obs.txt 2>> obs.err
	./nexus indexThenKey    "/tmp/ifile-rec-idxkey.dat"     >> obs.txt 2>> obs.err
	./nexus deepKeyChain    "/tmp/ifile-rec-deep.dat"       >> obs.txt 2>> obs.err
	./nexus mixedGroup      "/tmp/ifile-rec-mixed.dat"      >> obs.txt 2>> obs.err
	./nexus groupSkippingField "/tmp/ifile-rec-skip.dat"     >> obs.txt 2>> obs.err
	./nexus degenerateGroup "/tmp/ifile-rec-degen.dat"       >> obs.txt 2>> obs.err
	./nexus recordValuedKey "/tmp/ifile-rec-recval.dat"      >> obs.txt 2>> obs.err
	./nexus tupleValuedKey  "/tmp/ifile-rec-tupval.dat"      >> obs.txt 2>> obs.err
	./nexus keyBracketKey   "/tmp/ifile-rec-keybrkkey.dat"   >> obs.txt 2>> obs.err
	./nexus indexIndex      "/tmp/ifile-rec-idxidx.dat"      >> obs.txt 2>> obs.err
	rm -f /tmp/ifile-rec-*.dat

clean:
	rm -rf nexus pools __pycache__
	rm -f *.err
	rm -f /tmp/ifile-rec-*.dat
	rm -rf *-build *build.tmp*
