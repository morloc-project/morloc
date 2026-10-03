all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus guardedDiv 6 3 > obs.txt 2> obs.err
	./nexus guardedDiv 6 0 >> obs.txt 2>> obs.err
	./nexus guardedRec 4 >> obs.txt 2>> obs.err
	./nexus branched 6 0 >> obs.txt 2>> obs.err
	./nexus unreadSibling 5 >> obs.txt 2>> obs.err
	./nexus readTwice 5 >> obs.txt 2>> obs.err

clean:
	rm -f *.err nexus
	rm -rf *-build *build.tmp* __pycache__
