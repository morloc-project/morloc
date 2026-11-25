all:
	rm -f *.err obs.txt
	morloc make -o main main.loc 2>> build.err
	-./main openTwice 0 >> obs.txt 2>> obs.err
	-./main pinnedTwice 0 >> obs.txt 2>> obs.err
	-./main crossed 0 >> obs.txt 2>> obs.err

clean:
	rm -rf main __pycache__ *.err
	rm -rf *-build *build.tmp*
