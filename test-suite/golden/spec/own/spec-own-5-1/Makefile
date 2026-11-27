all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus second 3 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	./nexus nested 3 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	./nexus twoParts 3 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	./nexus wholeAndPart 3 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	./nexus twice 3 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	./nexus inTuple 3 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	./nexus spin 5 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	./nexus boxedTail 4 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	./nexus walkSum 4 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	./nexus consume 4 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt

clean:
	rm -rf nexus nexus-* pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
