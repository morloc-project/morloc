all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	rm -f tick.log; ./nexus onceDirect 5 > obs.txt 2> obs.err
	rm -f tick.log; ./nexus onceUnderLambda 5 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus twice 5 >> obs.txt 2>> obs.err
	if ./nexus unusedRaises 1 > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus branchRaises 1 > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus twiceRaises 1 > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	rm -f tick.log; ./nexus thunkArgOnce 5 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus nestedThunkArgOnce 5 >> obs.txt 2>> obs.err
	if ./nexus unusedFnRaises 1 > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	rm -f tick.log; ./nexus partialArgOnce 5 >> obs.txt 2>> obs.err
	morloc make -o pure pure.loc 2> pure-build.err
	if ./pure unusedThrows 1 > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./pure otherArmThrows 1 > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./pure unusedFnThrows 1 > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi

clean:
	rm -rf nexus pure __pycache__ *.err tick.log
	rm -rf *-build *build.tmp*
