all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus sumScale 3 4 >> obs.txt 2> obs.err
	./nexus sumScale 0 10 >> obs.txt 2>> obs.err
	./nexus listRepeat 3 [1,2] >> obs.txt 2>> obs.err
	./nexus listRepeat 0 [7,8,9] >> obs.txt 2>> obs.err
	./nexus twoCaptures 3 4 5 >> obs.txt 2>> obs.err
	./nexus guardUsesCapture 9 4 >> obs.txt 2>> obs.err
	./nexus guardUsesCapture 2 4 >> obs.txt 2>> obs.err
	./nexus unusedCapture 7 999 3 >> obs.txt 2>> obs.err
	./nexus pointFreeRec 3 4 >> obs.txt 2>> obs.err
	./nexus letBoundRec 3 4 >> obs.txt 2>> obs.err
	./nexus doubleRec 2 3 >> obs.txt 2>> obs.err
	./nexus transitiveWhere 3 4 >> obs.txt 2>> obs.err
	./nexus recAsHof 2 [0,1,3] >> obs.txt 2>> obs.err
	./nexus shadowCap 999 3 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err
