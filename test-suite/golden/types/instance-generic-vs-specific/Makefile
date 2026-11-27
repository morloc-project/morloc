all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus describeInt 1 > obs.txt 2> obs.err
	./nexus describeList '[1,2]' >> obs.txt 2>> obs.err
	./nexus describeVec '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus describeReal 1.5 >> obs.txt 2>> obs.err
	./nexus labelInt 1 >> obs.txt 2>> obs.err
	./nexus labelList '[1,2]' >> obs.txt 2>> obs.err
	./nexus labelVec '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus labelReal 1.5 >> obs.txt 2>> obs.err
	if morloc make -o ambiguous ambiguous.loc 2>> build.err; then echo "UNEXPECTED overlap accepted" >> obs.txt; else echo "OK overlap rejected" >> obs.txt; fi

clean:
	rm -rf nexus ambiguous pools __pycache__
	rm -rf *-build *build.tmp*
