all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus toList "[1,2,3]" 2>> obs.err >> obs.txt
	./nexus nameLen abcd 2>> obs.err >> obs.txt

clean:
	rm -rf nexus pools log __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
