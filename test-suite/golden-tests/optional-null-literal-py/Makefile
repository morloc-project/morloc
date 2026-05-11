all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus testListMixed > obs.txt 2> obs.err
	./nexus testListAllNull >> obs.txt 2>> obs.err
	./nexus testListEmpty >> obs.txt 2>> obs.err
	./nexus testListNested >> obs.txt 2>> obs.err
	./nexus testListReal >> obs.txt 2>> obs.err
	./nexus testRecordNullField >> obs.txt 2>> obs.err
	./nexus testRecordSomeField >> obs.txt 2>> obs.err
	./nexus testTupleNulls >> obs.txt 2>> obs.err
	./nexus --keep-null testNestedOptCollapseNull >> obs.txt 2>> obs.err
	./nexus testNestedOptCollapseSome >> obs.txt 2>> obs.err
	./nexus testRecordNestedOpt >> obs.txt 2>> obs.err
	./nexus testRoundtripDouble >> obs.txt 2>> obs.err
	./nexus testRoundtripCount >> obs.txt 2>> obs.err
	./nexus testRoundtripIdentity >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
