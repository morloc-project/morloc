all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus --keep-null testOptRecord > obs.txt 2> obs.err
	./nexus --keep-null testOptRecordNull >> obs.txt 2>> obs.err
	./nexus --keep-null testListRecords >> obs.txt 2>> obs.err
	./nexus --keep-null testRecordWithOpt >> obs.txt 2>> obs.err
	./nexus --keep-null testRecordWithOptNull >> obs.txt 2>> obs.err
	./nexus --keep-null testNestedRoundTrip >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
