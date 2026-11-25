all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus describeThree '[1,2,3]' > obs.txt 2> obs.err
	./nexus describeTwo '[1,2]' >> obs.txt 2>> obs.err
	./nexus labelThree '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus labelTwo '[1,2]' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
