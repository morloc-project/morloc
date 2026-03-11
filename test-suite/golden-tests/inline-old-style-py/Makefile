all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus testAdd 3 4 > obs.txt 2> obs.err
	./nexus testExpr 2 3 4 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err __pycache__
