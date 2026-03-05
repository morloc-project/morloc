all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus testStr > obs.txt 2> obs.err
	./nexus testList >> obs.txt 2>> obs.err
	./nexus testListInt >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__
