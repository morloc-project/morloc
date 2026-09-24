all:
	rm -f *.err obs.txt tick.log
	echo '--- py ---' >> obs.txt
	morloc make -o main-py main-py.loc 2>> build.err
	-rm -f tick.log; echo -n 'knownTwice ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py knownTwice 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'userFormTwice ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py userFormTwice 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'unknownThroughRecursion ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py unknownThroughRecursion 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'unusedPartial ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py unusedPartial 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'twoStagesOuter ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py twoStagesOuter 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'twoStagesInner ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py twoStagesInner 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'twoStagesFull ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py twoStagesFull 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'conditionalStage ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py conditionalStage 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'hostPartial ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py hostPartial 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'hostFull ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py hostFull 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'selfTwiceCount ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py selfTwiceCount 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'selfUnusedCount ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py selfUnusedCount 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'flatWhere ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py flatWhere 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'wrapWhere ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py wrapWhere 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'tripleThroughRec ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py tripleThroughRec 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'whereFunctionShared ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py whereFunctionShared 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'cafFunctionShared ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py cafFunctionShared 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'partialUnderLambda ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py partialUnderLambda 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'stagedRecAsValue ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py stagedRecAsValue 0 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'underArityUnknown ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py underArityUnknown 0 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'overTwoStagesUnknown ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py overTwoStagesUnknown 0 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'stagedInList ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py stagedInList 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'capturedRecStaged ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py capturedRecStaged 0 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'partialBeforeStageRec ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py partialBeforeStageRec 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'capturedPartialRec ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py capturedPartialRec 2 >> obs.txt 2>> obs.err
	if TICK_LOG=$(CURDIR)/tick.log ./main-py prefixFailsOutsideTry 0 > /dev/null 2>> obs.err; then echo 'UNEXPECTED prefixFailsOutsideTry' >> obs.txt; else echo 'OK prefixFailsOutsideTry' >> obs.txt; fi
	if TICK_LOG=$(CURDIR)/tick.log ./main-py partialIntoTry 0 > /dev/null 2>> obs.err; then echo 'UNEXPECTED partialIntoTry' >> obs.txt; else echo 'OK partialIntoTry' >> obs.txt; fi
	-rm -f tick.log; echo -n 'crossPool ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py crossPool 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'crossRecValue ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py crossRecValue 0 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'crossUnknown ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py crossUnknown 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'crossTwoStages ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py crossTwoStages 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'crossUnder ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py crossUnder 2 >> obs.txt 2>> obs.err
	echo '--- cpp ---' >> obs.txt
	morloc make -o main-cpp main-cpp.loc 2>> build.err
	-rm -f tick.log; echo -n 'knownTwice ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cpp knownTwice 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'userFormTwice ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cpp userFormTwice 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'unknownThroughRecursion ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cpp unknownThroughRecursion 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'unusedPartial ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cpp unusedPartial 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'twoStagesOuter ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cpp twoStagesOuter 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'twoStagesInner ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cpp twoStagesInner 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'twoStagesFull ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cpp twoStagesFull 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'conditionalStage ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cpp conditionalStage 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'hostPartial ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cpp hostPartial 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'hostFull ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cpp hostFull 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'selfTwiceCount ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cpp selfTwiceCount 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'selfUnusedCount ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cpp selfUnusedCount 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'flatWhere ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cpp flatWhere 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'wrapWhere ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cpp wrapWhere 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'tripleThroughRec ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cpp tripleThroughRec 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'whereFunctionShared ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cpp whereFunctionShared 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'cafFunctionShared ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cpp cafFunctionShared 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'partialUnderLambda ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cpp partialUnderLambda 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'stagedRecAsValue ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cpp stagedRecAsValue 0 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'underArityUnknown ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cpp underArityUnknown 0 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'overTwoStagesUnknown ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cpp overTwoStagesUnknown 0 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'stagedInList ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cpp stagedInList 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'capturedRecStaged ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cpp capturedRecStaged 0 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'partialBeforeStageRec ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cpp partialBeforeStageRec 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'capturedPartialRec ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cpp capturedPartialRec 2 >> obs.txt 2>> obs.err
	if TICK_LOG=$(CURDIR)/tick.log ./main-cpp prefixFailsOutsideTry 0 > /dev/null 2>> obs.err; then echo 'UNEXPECTED prefixFailsOutsideTry' >> obs.txt; else echo 'OK prefixFailsOutsideTry' >> obs.txt; fi
	if TICK_LOG=$(CURDIR)/tick.log ./main-cpp partialIntoTry 0 > /dev/null 2>> obs.err; then echo 'UNEXPECTED partialIntoTry' >> obs.txt; else echo 'OK partialIntoTry' >> obs.txt; fi
	echo '--- rust ---' >> obs.txt
	morloc make -o main-rust main-rust.loc 2>> build.err
	-rm -f tick.log; echo -n 'knownTwice ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-rust knownTwice 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'userFormTwice ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-rust userFormTwice 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'unknownThroughRecursion ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-rust unknownThroughRecursion 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'unusedPartial ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-rust unusedPartial 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'twoStagesOuter ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-rust twoStagesOuter 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'twoStagesInner ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-rust twoStagesInner 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'twoStagesFull ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-rust twoStagesFull 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'conditionalStage ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-rust conditionalStage 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'hostPartial ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-rust hostPartial 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'hostFull ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-rust hostFull 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'selfTwiceCount ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-rust selfTwiceCount 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'selfUnusedCount ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-rust selfUnusedCount 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'flatWhere ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-rust flatWhere 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'wrapWhere ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-rust wrapWhere 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'tripleThroughRec ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-rust tripleThroughRec 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'whereFunctionShared ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-rust whereFunctionShared 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'cafFunctionShared ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-rust cafFunctionShared 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'partialUnderLambda ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-rust partialUnderLambda 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'stagedRecAsValue ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-rust stagedRecAsValue 0 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'underArityUnknown ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-rust underArityUnknown 0 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'overTwoStagesUnknown ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-rust overTwoStagesUnknown 0 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'stagedInList ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-rust stagedInList 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'capturedRecStaged ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-rust capturedRecStaged 0 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'partialBeforeStageRec ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-rust partialBeforeStageRec 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'capturedPartialRec ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-rust capturedPartialRec 2 >> obs.txt 2>> obs.err
	if TICK_LOG=$(CURDIR)/tick.log ./main-rust prefixFailsOutsideTry 0 > /dev/null 2>> obs.err; then echo 'UNEXPECTED prefixFailsOutsideTry' >> obs.txt; else echo 'OK prefixFailsOutsideTry' >> obs.txt; fi
	if TICK_LOG=$(CURDIR)/tick.log ./main-rust partialIntoTry 0 > /dev/null 2>> obs.err; then echo 'UNEXPECTED partialIntoTry' >> obs.txt; else echo 'OK partialIntoTry' >> obs.txt; fi
	echo '--- r ---' >> obs.txt
	morloc make -o main-r main-r.loc 2>> build.err
	-rm -f tick.log; echo -n 'knownTwice ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-r knownTwice 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'userFormTwice ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-r userFormTwice 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'unknownThroughRecursion ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-r unknownThroughRecursion 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'unusedPartial ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-r unusedPartial 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'twoStagesOuter ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-r twoStagesOuter 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'twoStagesInner ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-r twoStagesInner 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'twoStagesFull ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-r twoStagesFull 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'conditionalStage ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-r conditionalStage 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'hostPartial ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-r hostPartial 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'hostFull ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-r hostFull 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'selfTwiceCount ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-r selfTwiceCount 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'selfUnusedCount ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-r selfUnusedCount 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'flatWhere ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-r flatWhere 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'wrapWhere ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-r wrapWhere 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'tripleThroughRec ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-r tripleThroughRec 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'whereFunctionShared ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-r whereFunctionShared 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'cafFunctionShared ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-r cafFunctionShared 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'partialUnderLambda ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-r partialUnderLambda 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'stagedRecAsValue ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-r stagedRecAsValue 0 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'underArityUnknown ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-r underArityUnknown 0 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'overTwoStagesUnknown ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-r overTwoStagesUnknown 0 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'stagedInList ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-r stagedInList 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'capturedRecStaged ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-r capturedRecStaged 0 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'partialBeforeStageRec ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-r partialBeforeStageRec 2 >> obs.txt 2>> obs.err
	-rm -f tick.log; echo -n 'capturedPartialRec ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-r capturedPartialRec 2 >> obs.txt 2>> obs.err
	if TICK_LOG=$(CURDIR)/tick.log ./main-r prefixFailsOutsideTry 0 > /dev/null 2>> obs.err; then echo 'UNEXPECTED prefixFailsOutsideTry' >> obs.txt; else echo 'OK prefixFailsOutsideTry' >> obs.txt; fi
	if TICK_LOG=$(CURDIR)/tick.log ./main-r partialIntoTry 0 > /dev/null 2>> obs.err; then echo 'UNEXPECTED partialIntoTry' >> obs.txt; else echo 'OK partialIntoTry' >> obs.txt; fi

clean:
	rm -rf main-py main-cpp main-rust main-r __pycache__ *.err tick.log
	rm -rf *-build *build.tmp*
