all:
	: > obs.txt
	rm -f *.err
	if morloc make -o nexus main.loc 2> build.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	morloc make -o ok ok.loc 2>> build.err
	./ok first '[4,5]' >> obs.txt 2>> obs.err
	morloc make -o run run.loc 2>> build.err
	./run first '[6,7]' >> obs.txt 2>> obs.err

clean:
	rm -f *.err nexus ok run
	rm -rf *-build *build.tmp* __pycache__
