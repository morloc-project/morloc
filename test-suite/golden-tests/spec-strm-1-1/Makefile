all:
	: > obs.txt
	rm -f *.err ints.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus writeInts ints.dat 2>> obs.err
	for f in openNexus openPy openCpp openR openRust streamNexus streamPy; do \
		printf '%s: ' $$f >> obs.txt; ./nexus $$f ints.dat >> obs.txt 2>> obs.err; \
	done
	for f in argNexus argPy argCpp argR argRust listPy listCpp listR listRust lieCpp; do \
		./nexus $$f ints.dat >> obs.txt 2>> obs.err; echo "$$f: exit $$?" >> obs.txt; \
	done
	./nexus intsNexus ints.dat >> obs.txt 2>> obs.err
	./nexus intsPy ints.dat >> obs.txt 2>> obs.err
	morloc make -o bad bad.loc 2>> build.err && echo "UNEXPECTED_SUCCESS" >> obs.txt || echo "bad: build_rejected" >> obs.txt

clean:
	rm -rf nexus bad pools log __pycache__ *.err ints.dat
	rm -rf *-build *build.tmp*
