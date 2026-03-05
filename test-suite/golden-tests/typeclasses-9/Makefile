all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus testSemiStr > obs.txt 2> obs.err
	./nexus testMonoidStr >> obs.txt 2>> obs.err
	./nexus testMonoidList >> obs.txt 2>> obs.err
	./nexus testGroupNegate >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__
