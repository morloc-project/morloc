all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- nestedOut ---" >> obs.txt
	./nexus nestedOut 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- nestedLoop ---" >> obs.txt
	./nexus nestedLoop 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- tagNested ---" >> obs.txt
	./nexus tagNested 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- tagField ---" >> obs.txt
	./nexus tagField 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- fieldArg ---" >> obs.txt
	./nexus fieldArg 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- fieldSrc ---" >> obs.txt
	./nexus fieldSrc 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- fieldSrcLit ---" >> obs.txt
	./nexus fieldSrcLit 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- fieldRet ---" >> obs.txt
	./nexus fieldRet 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- tupleArg ---" >> obs.txt
	./nexus tupleArg 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- tupleFresh ---" >> obs.txt
	./nexus tupleFresh 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- listPat ---" >> obs.txt
	./nexus listPat 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- listRet ---" >> obs.txt
	./nexus listRet 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- twiceRead ---" >> obs.txt
	./nexus twiceRead 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- twiceKeep ---" >> obs.txt
	./nexus twiceKeep 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
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
	echo "--- closureRead ---" >> obs.txt
	./nexus closureRead 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- closureKeep ---" >> obs.txt
	./nexus closureKeep 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- papRead ---" >> obs.txt
	./nexus papRead 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- papHof ---" >> obs.txt
	./nexus papHof 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- letOnce ---" >> obs.txt
	./nexus letOnce 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- letParam ---" >> obs.txt
	./nexus letParam 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- letField ---" >> obs.txt
	./nexus letField 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- matchUnwrap ---" >> obs.txt
	./nexus matchUnwrap 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- matchArg ---" >> obs.txt
	./nexus matchArg 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- matchTry ---" >> obs.txt
	./nexus matchTry 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- throughTwo ---" >> obs.txt
	./nexus throughTwo 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- throughLoop ---" >> obs.txt
	./nexus throughLoop 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- throughCli ---" >> obs.txt
	./nexus throughCli '{"Link":[1,{"Link":[2,"End"]}]}' >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- nonTail ---" >> obs.txt
	./nexus nonTail 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- guardTwice ---" >> obs.txt
	./nexus guardTwice 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
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

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
