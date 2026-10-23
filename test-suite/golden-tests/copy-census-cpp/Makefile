all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- baseBuild ---" >> obs.txt
	./nexus baseBuild 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- baseWalk ---" >> obs.txt
	./nexus baseWalk 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- nestedOut ---" >> obs.txt
	./nexus nestedOut 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- nestedLoop ---" >> obs.txt
	./nexus nestedLoop 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- nestedLoop2 ---" >> obs.txt
	./nexus nestedLoop2 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- tagNested ---" >> obs.txt
	./nexus tagNested 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- tagField ---" >> obs.txt
	./nexus tagField 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- fieldArg ---" >> obs.txt
	./nexus fieldArg 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- fieldArgM ---" >> obs.txt
	./nexus fieldArgM 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- fieldRet ---" >> obs.txt
	./nexus fieldRet 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- tupleArg ---" >> obs.txt
	./nexus tupleArg 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- listPat ---" >> obs.txt
	./nexus listPat 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- listRet ---" >> obs.txt
	./nexus listRet 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- listLit ---" >> obs.txt
	./nexus listLit 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- listRecords ---" >> obs.txt
	./nexus listRecords 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- listLoop ---" >> obs.txt
	./nexus listLoop 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- listStep ---" >> obs.txt
	./nexus listStep 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- twiceRead ---" >> obs.txt
	./nexus twiceRead 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- twiceLet ---" >> obs.txt
	./nexus twiceLet 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- twiceKeep ---" >> obs.txt
	./nexus twiceKeep 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- useAndKeep ---" >> obs.txt
	./nexus useAndKeep 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- twoSteps ---" >> obs.txt
	./nexus twoSteps 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- partsBorrowed ---" >> obs.txt
	./nexus partsBorrowed 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- partsFresh ---" >> obs.txt
	./nexus partsFresh 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- setter ---" >> obs.txt
	./nexus setter 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- setterFn ---" >> obs.txt
	./nexus setterFn 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- tlOnce ---" >> obs.txt
	./nexus tlOnce 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- tlLoopRead ---" >> obs.txt
	./nexus tlLoopRead 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- tlLoopKeep ---" >> obs.txt
	./nexus tlLoopKeep 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- tlLoopBuilt ---" >> obs.txt
	./nexus tlLoopBuilt 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- tlLoopInline ---" >> obs.txt
	./nexus tlLoopInline 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- closureRead ---" >> obs.txt
	./nexus closureRead 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- closureFn ---" >> obs.txt
	./nexus closureFn 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- closureRef ---" >> obs.txt
	./nexus closureRef 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- closureDo ---" >> obs.txt
	./nexus closureDo 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- papRead ---" >> obs.txt
	./nexus papRead 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- papHof ---" >> obs.txt
	./nexus papHof 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- letOnce ---" >> obs.txt
	./nexus letOnce 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- letKeep ---" >> obs.txt
	./nexus letKeep 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- letSource ---" >> obs.txt
	./nexus letSource 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- matchCond ---" >> obs.txt
	./nexus matchCond 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- matchCondFresh ---" >> obs.txt
	./nexus matchCondFresh 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- matchRec ---" >> obs.txt
	./nexus matchRec 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- matchUnwrap ---" >> obs.txt
	./nexus matchUnwrap 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- matchTry ---" >> obs.txt
	./nexus matchTry 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- throughTwo ---" >> obs.txt
	./nexus throughTwo 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- throughLoop ---" >> obs.txt
	./nexus throughLoop 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- loopFresh ---" >> obs.txt
	./nexus loopFresh 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- loopGrow ---" >> obs.txt
	./nexus loopGrow 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- loopCarried ---" >> obs.txt
	./nexus loopCarried 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- loopField ---" >> obs.txt
	./nexus loopField 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- loopTuple ---" >> obs.txt
	./nexus loopTuple 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- tupleStep ---" >> obs.txt
	./nexus tupleStep 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- loopLet ---" >> obs.txt
	./nexus loopLet 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- loopLetTwice ---" >> obs.txt
	./nexus loopLetTwice 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- loopLetAlias ---" >> obs.txt
	./nexus loopLetAlias 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- nonTail ---" >> obs.txt
	./nexus nonTail 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- guardLoop ---" >> obs.txt
	./nexus guardLoop 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- guardKeep ---" >> obs.txt
	./nexus guardKeep 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- mixedLoop ---" >> obs.txt
	./nexus mixedLoop 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- matchedTwice ---" >> obs.txt
	./nexus matchedTwice 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- twoLets ---" >> obs.txt
	./nexus twoLets 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- aliasKeep ---" >> obs.txt
	./nexus aliasKeep 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
