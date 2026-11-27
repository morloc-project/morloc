all:
	rm -f *.err obs.txt
	morloc make -o main main.loc 2>> build.err
	-./main ladder 0 >> obs.txt 2>> obs.err
	if ./main ignored 1 >> obs.err 2>&1; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi

clean:
	rm -rf main main-build main-build.tmp* *.err
