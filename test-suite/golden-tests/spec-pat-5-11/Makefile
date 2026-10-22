all:
	: > obs.txt
	rm -f *.err
	if morloc make -o nexus main.loc 2> build.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	morloc make -o ok ok.loc 2>> build.err
	./ok first '[4,5]' >> obs.txt 2>> obs.err

clean:
	rm -f *.err nexus ok
	rm -rf *-build *build.tmp* __pycache__
