all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus testPureLit > obs.txt 2> obs.err
	./nexus testPureExpr >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
