all:
	rm -f *.err obs.txt
	morloc make -o py main-py.loc 2>> build.err
	morloc make -o cpp main-cpp.loc 2>> build.err
	morloc make -o rust main-rust.loc 2>> build.err
	./py x 3 >> obs.txt 2>> obs.err
	./py y 3 >> obs.txt 2>> obs.err
	./py z 3 >> obs.txt 2>> obs.err
	./py inList 3 >> obs.txt 2>> obs.err
	./py inTuple 3 >> obs.txt 2>> obs.err
	./cpp x 3 >> obs.txt 2>> obs.err
	./cpp y 3 >> obs.txt 2>> obs.err
	./cpp z 3 >> obs.txt 2>> obs.err
	./cpp inList 3 >> obs.txt 2>> obs.err
	./cpp inTuple 3 >> obs.txt 2>> obs.err
	./rust x 3 >> obs.txt 2>> obs.err
	./rust y 3 >> obs.txt 2>> obs.err
	./rust z 3 >> obs.txt 2>> obs.err
	./rust inList 3 >> obs.txt 2>> obs.err
	./rust inTuple 3 >> obs.txt 2>> obs.err

clean:
	rm -rf py cpp rust __pycache__ *.err
	rm -rf *-build *build.tmp*
