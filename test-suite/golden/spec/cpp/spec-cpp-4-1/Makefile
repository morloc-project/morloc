all:
	rm -f *.err obs.txt
	morloc make -o bad-ctor bad-ctor.loc 2> build-ctor.err && echo "UNEXPECTED_SUCCESS ctor" >> obs.txt || (grep -q "CPP-4" build-ctor.err && echo "rejected ctor" || echo "UNEXPECTED_ERROR ctor") >> obs.txt
	morloc make -o nexus ok.loc 2> build.err
	./nexus agg 3 >> obs.txt 2>> obs.err
	./nexus byValue 3 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus bad-ctor pools __pycache__
	rm -f *.err
	rm -rf *-build *build.tmp*
