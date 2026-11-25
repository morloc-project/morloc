all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus shapeInt '[[1,2],[3,4]]' > obs.txt 2> obs.err
	./nexus shapeStr '[["a",2],["b",4]]' >> obs.txt 2>> obs.err
	./nexus shapeName '[["c",1]]' >> obs.txt 2>> obs.err
	./nexus roundInt '[[1,2],[3,4]]' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
