all:
	rm -f *.err obs.txt
	if morloc make -o nexus main.loc 2> build.err; then echo UNEXPECTED > obs.txt; else echo OK > obs.txt; fi

clean:
	rm -rf nexus pools *.err __pycache__
	rm -rf *-build *build.tmp*
