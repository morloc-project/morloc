all:
	rm -f *.err obs.txt
	morloc make -o main-py main-py.loc 2>> build.err
	-./main-py recThroughRecursion 2 >> obs.txt 2>> obs.err
	-./main-py recToFunction 10 >> obs.txt 2>> obs.err
	morloc make -o main-cpp main-cpp.loc 2>> build.err
	-./main-cpp recThroughRecursion 2 >> obs.txt 2>> obs.err
	-./main-cpp recToFunction 10 >> obs.txt 2>> obs.err
	morloc make -o main-rust main-rust.loc 2>> build.err
	-./main-rust recThroughRecursion 2 >> obs.txt 2>> obs.err
	-./main-rust recToFunction 10 >> obs.txt 2>> obs.err
	morloc make -o main-r main-r.loc 2>> build.err
	-./main-r recThroughRecursion 2 >> obs.txt 2>> obs.err
	-./main-r recToFunction 10 >> obs.txt 2>> obs.err

clean:
	rm -rf main-py main-cpp main-rust main-r __pycache__ *.err
	rm -rf *-build *build.tmp*
