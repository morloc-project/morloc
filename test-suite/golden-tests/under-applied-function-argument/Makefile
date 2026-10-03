all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus zip3 '[1,2]' '[10,20]' '[100,200]' > obs.txt 2> obs.err
	./nexus curried '[2,3]' '[10,20]' >> obs.txt 2>> obs.err
	./nexus crossLang '[1,2]' '[3,4]' '[5,6]' >> obs.txt 2>> obs.err
	./nexus sectioned '[1,2]' '[10,20]' >> obs.txt 2>> obs.err

clean:
	rm -f *.err nexus
	rm -rf *-build *build.tmp* __pycache__
