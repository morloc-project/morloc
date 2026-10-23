all:
	: > obs.txt
	rm -f *.err z0.bin z3.bin
	morloc make -o nexus main.loc 2> build.err
	./nexus save0 z0.bin '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus save3 z3.bin '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus loadInts z0.bin >> obs.txt 2>> obs.err
	./nexus loadInts z3.bin >> obs.txt 2>> obs.err
	./nexus loadReals z0.bin 2>&1 | grep -c '"Err"' >> obs.txt
	./nexus loadReals z3.bin 2>&1 | grep -c '"Err"' >> obs.txt
	./nexus sumInts z3.bin >> obs.txt 2>> obs.err
	if ./nexus sumReals z3.bin > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi

clean:
	rm -f *.err z0.bin z3.bin nexus
	rm -rf *-build *build.tmp*
