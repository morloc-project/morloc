all:
	rm -f *.err data.mlc stream.mlc
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus writeData data.mlc >> obs.txt 2>> obs.err
	./nexus writeStream stream.mlc >> obs.txt 2>> obs.err
	./nexus second data.mlc >> obs.txt 2>> obs.err
	./nexus second stream.mlc >> obs.txt 2>> obs.err
	./nexus secondPy data.mlc >> obs.txt 2>> obs.err
	./nexus secondPy stream.mlc >> obs.txt 2>> obs.err
	./nexus count data.mlc stream.mlc >> obs.txt 2>> obs.err
	if ./nexus count data.mlc /no/such/file.mlc 2> tmp.err; then echo UNEXPECTED >> obs.txt; else grep -oE "cannot open IFile '/no/such/file.mlc'" tmp.err >> obs.txt; fi
	if ./nexus count data.mlc - < /dev/null 2> tmp.err; then echo UNEXPECTED >> obs.txt; else grep -oE "an IFile cannot be read from stdin" tmp.err >> obs.txt; fi
	./nexus pick '{"f":"data.mlc","k":1}' >> obs.txt 2>> obs.err
	if ./nexus pick '{"f":"/no/such/r.mlc","k":1}' 2> tmp.err; then echo UNEXPECTED >> obs.txt; else grep -oE "cannot open IFile '/no/such/r.mlc'" tmp.err >> obs.txt; fi
	rm -f tmp.err
	rm -f data.mlc stream.mlc

clean:
	rm -rf nexus pools __pycache__ *.err data.mlc stream.mlc
	rm -rf *-build *build.tmp*
