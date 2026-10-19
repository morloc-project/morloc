all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus sameOrder 2>> obs.err >> obs.txt
	./nexus otherOrder 2>> obs.err >> obs.txt

clean:
	rm -rf nexus pools log __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
