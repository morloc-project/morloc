all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus drainP [1,2,3,4] 0 >> obs.txt 2>> obs.err
	./nexus drainP [10,20,30] 0 >> obs.txt 2>> obs.err
	./nexus drainC [1,2,3,4,5] 0 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
