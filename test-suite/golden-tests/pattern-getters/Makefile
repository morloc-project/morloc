all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus testr   >  obs.txt
	./nexus testpy  2>> obs.err  >> obs.txt
	./nexus testcpp 2>> obs.err  >> obs.txt
	./nexus pureNestedTuple    2>> obs.err >> obs.txt
	./nexus pureNested3Tuple   2>> obs.err >> obs.txt
	./nexus pureScalar         2>> obs.err >> obs.txt
	./nexus pureGrouped        2>> obs.err >> obs.txt
	./nexus pureChained        2>> obs.err >> obs.txt
	./nexus pureSingleGrouped  2>> obs.err >> obs.txt
	./nexus pureChainedScalar  2>> obs.err >> obs.txt
	./nexus pureRecordHetGetter   2>> obs.err >> obs.txt
	./nexus pureRecordSingleField 2>> obs.err >> obs.txt
	./nexus pureRecordKeyChain    2>> obs.err >> obs.txt
	./nexus pureFieldThenBracketOnListRec 2>> obs.err >> obs.txt

clean:
	rm -rf nexus pools __pycache__
