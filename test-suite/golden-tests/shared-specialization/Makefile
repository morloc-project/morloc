all:
	rm -f *.err obs.txt tick.log
	morloc make -o main-py main-py.loc 2>> build.err
	-./main-py intLadder 0 >> obs.txt 2>> obs.err
	-./main-py listLadder '[1,2,3]' >> obs.txt 2>> obs.err
	-./main-py recursiveLeafLadder 0 >> obs.txt 2>> obs.err
	-./main-py higherOrderLadder 0 >> obs.txt 2>> obs.err
	-./main-py swapLadder 1 2 >> obs.txt 2>> obs.err
	-./main-py curriedLadder 0 >> obs.txt 2>> obs.err
	-./main-py memptyLadder x | wc -c | tr -d ' ' >> obs.txt 2>> obs.err
	-rm -f tick.log; TICK_LOG=$(CURDIR)/tick.log ./main-py constantLadder 0 >> obs.txt 2>> obs.err
	-rm -f tick.log; TICK_LOG=$(CURDIR)/tick.log ./main-py partialConstantLadder 0 >> obs.txt 2>> obs.err
	-./main-py tupleLadder 3 >> obs.txt 2>> obs.err
	-./main-py pureTuple 3 >> obs.txt 2>> obs.err
	morloc make -o main-cpp main-cpp.loc 2>> build.err
	-./main-cpp intLadder 0 >> obs.txt 2>> obs.err
	-./main-cpp listLadder '[1,2,3]' >> obs.txt 2>> obs.err
	-./main-cpp recursiveLeafLadder 0 >> obs.txt 2>> obs.err
	-./main-cpp higherOrderLadder 0 >> obs.txt 2>> obs.err
	-./main-cpp swapLadder 1 2 >> obs.txt 2>> obs.err
	-./main-cpp curriedLadder 0 >> obs.txt 2>> obs.err
	morloc make -o main-rust main-rust.loc 2>> build.err
	-./main-rust intLadder 0 >> obs.txt 2>> obs.err
	-./main-rust listLadder '[1,2,3]' >> obs.txt 2>> obs.err
	-./main-rust recursiveLeafLadder 0 >> obs.txt 2>> obs.err
	-./main-rust higherOrderLadder 0 >> obs.txt 2>> obs.err
	-./main-rust swapLadder 1 2 >> obs.txt 2>> obs.err
	-./main-rust curriedLadder 0 >> obs.txt 2>> obs.err
	morloc make -o main-r main-r.loc 2>> build.err
	-./main-r intLadder 0 >> obs.txt 2>> obs.err
	-./main-r listLadder '[1,2,3]' >> obs.txt 2>> obs.err
	-./main-r recursiveLeafLadder 0 >> obs.txt 2>> obs.err
	-./main-r higherOrderLadder 0 >> obs.txt 2>> obs.err
	-./main-r swapLadder 1 2 >> obs.txt 2>> obs.err
	-./main-r curriedLadder 0 >> obs.txt 2>> obs.err
	morloc make -o main-mixed main-mixed.loc 2>> build.err
	-./main-mixed inPy 7 >> obs.txt 2>> obs.err
	-./main-mixed inCpp 7 >> obs.txt 2>> obs.err
	-./main-mixed closureLadder 0 >> obs.txt 2>> obs.err

clean:
	rm -rf main-py main-cpp main-rust main-r main-mixed __pycache__ *.err tick.log
	rm -rf *-build *build.tmp*
