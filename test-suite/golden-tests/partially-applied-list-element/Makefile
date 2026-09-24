all:
	rm -f *.err obs.txt
	morloc make -o main-py main-py.loc 2>> build.err
	-./main-py applyAll 10 >> obs.txt 2>> obs.err
	-./main-py applyAllCurried 10 >> obs.txt 2>> obs.err
	morloc make -o main-cpp main-cpp.loc 2>> build.err
	-./main-cpp applyAll 10 >> obs.txt 2>> obs.err
	-./main-cpp applyAllCurried 10 >> obs.txt 2>> obs.err
	morloc make -o main-rust main-rust.loc 2>> build.err
	-./main-rust applyAll 10 >> obs.txt 2>> obs.err
	-./main-rust applyAllCurried 10 >> obs.txt 2>> obs.err
	morloc make -o main-r main-r.loc 2>> build.err
	-./main-r applyAll 10 >> obs.txt 2>> obs.err
	-./main-r applyAllCurried 10 >> obs.txt 2>> obs.err

clean:
	rm -rf main-py main-cpp main-rust main-r __pycache__ *.err
	rm -rf *-build *build.tmp*
