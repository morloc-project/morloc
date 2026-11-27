all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus flipBools '[]' >> obs.txt 2>> obs.err
	./nexus flipBools '[true]' >> obs.txt 2>> obs.err
	./nexus flipBools '[true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false]' >> obs.txt 2>> obs.err
	./nexus flipBools '[true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false]' >> obs.txt 2>> obs.err
	./nexus flipBools '[true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false,true,false,true,false,false]' >> obs.txt 2>> obs.err
	./nexus flipPyBools 0 >> obs.txt 2>> obs.err
	./nexus flipPyBools 63 >> obs.txt 2>> obs.err
	./nexus flipPyBools 64 >> obs.txt 2>> obs.err
	./nexus flipPyBools 129 >> obs.txt 2>> obs.err
	./nexus bumpInts '[-2147483648,0,2147483646]' >> obs.txt 2>> obs.err
	if ./nexus bumpInts '[1,4294967296]' >> obs.txt 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi

clean:
	rm -rf nexus pools *.err
	rm -rf *-build *build.tmp*
