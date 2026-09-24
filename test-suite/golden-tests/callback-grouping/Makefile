all:
	rm -f *.err obs.txt
	morloc make -o main-py main-py.loc 2>> build.err
	-./main-py curriedDef 4 >> obs.txt 2>> obs.err
	-./main-py flatDef 4 >> obs.txt 2>> obs.err
	-./main-py stagedDef 4 >> obs.txt 2>> obs.err
	-./main-py threeGroups 4 >> obs.txt 2>> obs.err
	morloc make -o main-cpp main-cpp.loc 2>> build.err
	-./main-cpp curriedDef 4 >> obs.txt 2>> obs.err
	-./main-cpp flatDef 4 >> obs.txt 2>> obs.err
	-./main-cpp stagedDef 4 >> obs.txt 2>> obs.err
	-./main-cpp threeGroups 4 >> obs.txt 2>> obs.err
	morloc make -o main-rust main-rust.loc 2>> build.err
	-./main-rust curriedDef 4 >> obs.txt 2>> obs.err
	-./main-rust flatDef 4 >> obs.txt 2>> obs.err
	-./main-rust stagedDef 4 >> obs.txt 2>> obs.err
	-./main-rust threeGroups 4 >> obs.txt 2>> obs.err
	morloc make -o main-r main-r.loc 2>> build.err
	-./main-r curriedDef 4 >> obs.txt 2>> obs.err
	-./main-r flatDef 4 >> obs.txt 2>> obs.err
	-./main-r stagedDef 4 >> obs.txt 2>> obs.err
	-./main-r threeGroups 4 >> obs.txt 2>> obs.err
	if morloc make -o bad bad.loc > /dev/null 2>> obs.err; then echo 'UNEXPECTED bad' >> obs.txt; else echo 'OK bad' >> obs.txt; fi

clean:
	rm -rf main-py main-cpp main-rust main-r bad __pycache__ *.err
	rm -rf *-build *build.tmp*
