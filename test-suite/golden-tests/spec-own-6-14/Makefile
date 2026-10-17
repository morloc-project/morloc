all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- nested ---" >> obs.txt
	./nexus nested 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- nestedG ---" >> obs.txt
	./nexus nestedG 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- owned ---" >> obs.txt
	./nexus owned 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- ownedG ---" >> obs.txt
	./nexus ownedG 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- orNew ---" >> obs.txt
	./nexus orNew 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- orNewG ---" >> obs.txt
	./nexus orNewG 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- uses ---" >> obs.txt
	./nexus uses 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- usesG ---" >> obs.txt
	./nexus usesG 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- cheap ---" >> obs.txt
	./nexus cheap 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- cheapG ---" >> obs.txt
	./nexus cheapG 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- taken ---" >> obs.txt
	./nexus taken 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- takenG ---" >> obs.txt
	./nexus takenG 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- takenRec ---" >> obs.txt
	./nexus takenRec 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- loopOwned ---" >> obs.txt
	./nexus loopOwned 3 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- loopFields ---" >> obs.txt
	./nexus loopFields 3 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- loopFn ---" >> obs.txt
	./nexus loopFn 3 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- loopPap ---" >> obs.txt
	./nexus loopPap 2 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- loopClose ---" >> obs.txt
	./nexus loopClose 2 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- loopArgIf ---" >> obs.txt
	./nexus loopArgIf 4 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- loopSusp ---" >> obs.txt
	./nexus loopSusp 3 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- keepBoth ---" >> obs.txt
	./nexus keepBoth 3 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	echo "--- keepBothLoop ---" >> obs.txt
	./nexus keepBothLoop 3 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt

clean:
	rm -rf nexus nexus-* pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
