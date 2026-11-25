all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	> obs.txt
	./nexus okInt 42 2>> obs.err >> obs.txt
	./nexus okI64 42 2>> obs.err >> obs.txt
	./nexus okU8 42 2>> obs.err >> obs.txt
	./nexus okI8 -42 2>> obs.err >> obs.txt
	./nexus okU32 42 2>> obs.err >> obs.txt
	./nexus okF32 1.5 2>> obs.err >> obs.txt
	./nexus okF64 1.5 2>> obs.err >> obs.txt
	./nexus okF32FromInt 1.5 2>> obs.err >> obs.txt
	./nexus okAlias 42 2>> obs.err >> obs.txt
	./nexus okStr '"hi"' 2>> obs.err >> obs.txt
	./nexus failInt 2>> obs.err >> obs.txt
	./nexus failI64 2>> obs.err >> obs.txt
	./nexus failU8 2>> obs.err >> obs.txt
	./nexus failF32 2>> obs.err >> obs.txt
	./nexus failF32FromInt 2>> obs.err >> obs.txt
	./nexus failAlias 2>> obs.err >> obs.txt
	./nexus failStr 2>> obs.err >> obs.txt
	./nexus narrow 200 2>> obs.err >> obs.txt
	./nexus narrow -1 2>> obs.err >> obs.txt
	./nexus narrow 300 2>> obs.err >> obs.txt
	./nexus chained 2>> obs.err >> obs.txt
	./nexus effRow 42 2>> obs.err >> obs.txt

clean:
	rm -rf nexus __pycache__
	rm -f *.err
	rm -rf *-build *build.tmp*
