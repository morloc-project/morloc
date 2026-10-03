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
	rm -f data.mlc stream.mlc

clean:
	rm -rf nexus pools __pycache__ *.err data.mlc stream.mlc
	rm -rf *-build *build.tmp*
