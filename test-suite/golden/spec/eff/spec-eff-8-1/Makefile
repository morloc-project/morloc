all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus sayPy '[1,2,3]' > obs.txt 2>> obs.err
	./nexus sayCpp '[4,5]' >> obs.txt 2>> obs.err
	./nexus sayR '[6,7]' >> obs.txt 2>> obs.err
	./nexus sayRust '[8,9]' >> obs.txt 2>> obs.err
	./nexus sayPy '[]' >> obs.txt 2>> obs.err
	./nexus sumPy '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus sumCpp '[10,20]' >> obs.txt 2>> obs.err
	./nexus sumPure '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus logPy '[1,2]' >> obs.txt 2>> obs.err
	./nexus unitPy 0 >> obs.txt 2>> obs.err
	./nexus sumNone '[1,2]' >> obs.txt 2>> obs.err
	cat obs.err >> obs.txt

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
