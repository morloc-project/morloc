all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus evenPy 10 > obs.txt 2> obs.err
	./nexus evenPy 7 >> obs.txt 2>> obs.err
	./nexus evenCpp 7 >> obs.txt 2>> obs.err
	./nexus nestedPy '[[1,2],[3]]' >> obs.txt 2>> obs.err
	./nexus nestedPy '[[1,2],[4]]' >> obs.txt 2>> obs.err
	./nexus nestedCpp '[[1,2],[3]]' >> obs.txt 2>> obs.err
	./nexus nestedCpp '[[1],[3]]' >> obs.txt 2>> obs.err
	./nexus nestedNePy '[[[1]],[[2,3]]]' >> obs.txt 2>> obs.err
	./nexus nestedNePy '[[[1]],[[2]]]' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
