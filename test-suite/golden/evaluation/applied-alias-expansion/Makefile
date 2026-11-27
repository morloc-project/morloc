all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus boxX ab >> obs.txt 2>> obs.err
	./nexus boxY ab >> obs.txt 2>> obs.err
	./nexus bqBases [1,2,3] [4,5,6] >> obs.txt 2>> obs.err
	./nexus bqQuals [1,2,3] [4,5,6] >> obs.txt 2>> obs.err
	./nexus seqHeader r1 [1,2] [3,4] >> obs.txt 2>> obs.err
	./nexus seqQuals r1 [1,2] [3,4] >> obs.txt 2>> obs.err
	./nexus tagOf t [7,8] >> obs.txt 2>> obs.err
	./nexus tagValues t [7,8] >> obs.txt 2>> obs.err
	./nexus constY ab >> obs.txt 2>> obs.err
	./nexus labeledY ab >> obs.txt 2>> obs.err
	./nexus relabelY ab >> obs.txt 2>> obs.err
	./nexus countOf a b >> obs.txt 2>> obs.err
	./nexus scoreOf ab >> obs.txt 2>> obs.err
	./nexus wrap s >> obs.txt 2>> obs.err
	./nexus optBox o >> obs.txt 2>> obs.err
	./nexus optLit o >> obs.txt 2>> obs.err
	./nexus pickY true ab >> obs.txt 2>> obs.err
	./nexus pickY false ab >> obs.txt 2>> obs.err
	./nexus holderY ab >> obs.txt 2>> obs.err
	./nexus sizeOf ab >> obs.txt 2>> obs.err
	./nexus strScore ab >> obs.txt 2>> obs.err
	./nexus plainScoreOf ab >> obs.txt 2>> obs.err
	./nexus directY ab >> obs.txt 2>> obs.err
	./nexus plainQ ab >> obs.txt 2>> obs.err
	./nexus actOut 4 >> obs.txt 2>> obs.err
	./nexus actIn 4 >> obs.txt 2>> obs.err
	./nexus libOut 4 >> obs.txt 2>> obs.err
	./nexus scorePair 3 x >> obs.txt 2>> obs.err
	./nexus countDown 3 >> obs.txt 2>> obs.err
	./nexus useScored q >> obs.txt 2>> obs.err
	./nexus applyGrouped 3 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
