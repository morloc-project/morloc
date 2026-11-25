all:
	rm -f *.err obs.txt
	if morloc make -o nexus main.loc 2> build.err; then echo UNEXPECTED > obs.txt; else echo OK > obs.txt; fi
	morloc make -o local local.loc 2>> build.err
	./local leafSum 7 >> obs.txt 2> obs.err

clean:
	rm -rf nexus local pools __pycache__
	rm -rf *-build *build.tmp*
