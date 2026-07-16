all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus test       >> obs.txt 2>> obs.err
	./nexus testFinal  >> obs.txt 2>> obs.err
	./nexus testNested >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
