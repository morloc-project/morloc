all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus testOptRecord > obs.txt 2> obs.err
	./nexus testOptRecordNull >> obs.txt 2>> obs.err
	./nexus testListRecords >> obs.txt 2>> obs.err
	./nexus testRecordWithOpt >> obs.txt 2>> obs.err
	./nexus testRecordWithOptNull >> obs.txt 2>> obs.err
	./nexus testNestedRoundTrip >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *err
