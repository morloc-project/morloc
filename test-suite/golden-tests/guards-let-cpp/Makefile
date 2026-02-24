all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus absLet 5 > obs.txt 2> obs.err
	./nexus absLet -- -3 >> obs.txt 2>> obs.err
	./nexus absLet 0 >> obs.txt 2>> obs.err
	./nexus classifyDo 5 >> obs.txt 2>> obs.err
	./nexus classifyDo 50 >> obs.txt 2>> obs.err
	./nexus classifyDo 200 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
