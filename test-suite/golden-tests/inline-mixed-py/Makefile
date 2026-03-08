all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	-./nexus mixedOps 3 4 2 3 > obs.txt 2> obs.err

clean:
	rm -rf nexus pools *err __pycache__
