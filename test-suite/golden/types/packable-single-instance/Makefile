all:
	rm -f *.err obs.txt
	morloc make -o atomic atomic.loc 2> build.err
	morloc make -o nested nested.loc 2>> build.err
	morloc make -o nat nat.loc 2>> build.err
	echo "--- atomic ---" >> obs.txt
	./atomic howBlob '[1,2]' >> obs.txt 2> obs.err
	echo "--- nested ---" >> obs.txt
	./nested howOuter '[[1,2],[3]]' >> obs.txt 2>> obs.err
	echo "--- nat ---" >> obs.txt
	./nat howVec '[1,2,3]' >> obs.txt 2>> obs.err

clean:
	rm -f *.err
	rm -rf atomic nested nat __pycache__
	rm -rf *-build *build.tmp*
