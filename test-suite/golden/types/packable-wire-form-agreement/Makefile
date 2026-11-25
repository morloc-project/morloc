all:
	rm -f *.err obs.txt
	if morloc make -o disagree disagree.loc 2> build.err; then echo "UNEXPECTED disagreeing wire forms accepted" >> obs.txt; else echo "OK disagreeing wire forms rejected" >> obs.txt; fi
	morloc make -o agree agree.loc 2>> build.err
	./agree roundTrip '[3,4]' >> obs.txt 2> obs.err

clean:
	rm -f *.err
	rm -rf disagree agree __pycache__
	rm -rf *-build *build.tmp*
