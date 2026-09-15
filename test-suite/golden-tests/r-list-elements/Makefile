all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus roundtrip '[1,null,3]' >> obs.txt 2>> obs.err
	./nexus roundtrip '[null]' >> obs.txt 2>> obs.err
	./nexus roundtripStrs '["a"]' >> obs.txt 2>> obs.err
	./nexus roundtripStrs '["ab","c"]' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
