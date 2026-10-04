all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus pyForkReturns 1 >> obs.txt 2>> obs.err
	./nexus pyForkRaises 1 >> obs.txt 2>> obs.err
	./nexus pyForkExits 1 >> obs.txt 2>> obs.err
	./nexus cppForkReturns 1 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err __pycache__
	rm -rf *-build *build.tmp*
