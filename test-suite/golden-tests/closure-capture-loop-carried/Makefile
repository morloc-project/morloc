all:
	rm -f *.err obs.txt
	morloc make -o main-r main-r.loc 2> build.err
	morloc make -o main-py main-py.loc 2>> build.err
	morloc make -o main-cpp main-cpp.loc 2>> build.err
	-./main-r start 2 > obs.txt 2> obs.err
	-./main-py start 2 >> obs.txt 2>> obs.err
	-./main-cpp start 2 >> obs.txt 2>> obs.err
	-./main-r deepPass 100000 >> obs.txt 2>> obs.err
	-./main-py deepPass 100000 >> obs.txt 2>> obs.err
	-./main-cpp deepPass 100000 >> obs.txt 2>> obs.err

clean:
	rm -rf main-r main-py main-cpp __pycache__ *.err
	rm -rf *-build *build.tmp*
