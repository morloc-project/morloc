all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus flat '[[2,3],[1,2,3,4,5,6]]' 2>> obs.err >> obs.txt

clean:
	rm -rf nexus pools log __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
