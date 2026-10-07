all:
	: > obs.txt
	rm -f *.err
	morloc make -o py-cpp py-cpp.loc 2>> build.err
	echo "--- py-cpp ---" >> obs.txt
	./py-cpp one 3 >> obs.txt 2>> obs.err
	./py-cpp two 3 >> obs.txt 2>> obs.err
	./py-cpp many 3 >> obs.txt 2>> obs.err
	./py-cpp ops 3 >> obs.txt 2>> obs.err
	./py-cpp boxed 3 >> obs.txt 2>> obs.err
	./py-cpp plain 3 >> obs.txt 2>> obs.err
	./py-cpp nada 3 >> obs.txt 2>> obs.err
	morloc make -o cpp-py cpp-py.loc 2>> build.err
	echo "--- cpp-py ---" >> obs.txt
	./cpp-py one 3 >> obs.txt 2>> obs.err
	./cpp-py two 3 >> obs.txt 2>> obs.err
	./cpp-py many 3 >> obs.txt 2>> obs.err
	./cpp-py ops 3 >> obs.txt 2>> obs.err
	./cpp-py boxed 3 >> obs.txt 2>> obs.err
	./cpp-py plain 3 >> obs.txt 2>> obs.err
	./cpp-py nada 3 >> obs.txt 2>> obs.err
	morloc make -o py-r py-r.loc 2>> build.err
	echo "--- py-r ---" >> obs.txt
	./py-r one 3 >> obs.txt 2>> obs.err
	./py-r two 3 >> obs.txt 2>> obs.err
	./py-r many 3 >> obs.txt 2>> obs.err
	./py-r ops 3 >> obs.txt 2>> obs.err
	./py-r boxed 3 >> obs.txt 2>> obs.err
	./py-r plain 3 >> obs.txt 2>> obs.err
	./py-r nada 3 >> obs.txt 2>> obs.err
	morloc make -o r-py r-py.loc 2>> build.err
	echo "--- r-py ---" >> obs.txt
	./r-py one 3 >> obs.txt 2>> obs.err
	./r-py two 3 >> obs.txt 2>> obs.err
	./r-py many 3 >> obs.txt 2>> obs.err
	./r-py ops 3 >> obs.txt 2>> obs.err
	./r-py boxed 3 >> obs.txt 2>> obs.err
	./r-py plain 3 >> obs.txt 2>> obs.err
	./r-py nada 3 >> obs.txt 2>> obs.err
	morloc make -o cpp-r cpp-r.loc 2>> build.err
	echo "--- cpp-r ---" >> obs.txt
	./cpp-r one 3 >> obs.txt 2>> obs.err
	./cpp-r two 3 >> obs.txt 2>> obs.err
	./cpp-r many 3 >> obs.txt 2>> obs.err
	./cpp-r ops 3 >> obs.txt 2>> obs.err
	./cpp-r boxed 3 >> obs.txt 2>> obs.err
	./cpp-r plain 3 >> obs.txt 2>> obs.err
	./cpp-r nada 3 >> obs.txt 2>> obs.err

clean:
	rm -rf py-cpp cpp-py py-r r-py cpp-r __pycache__ *.err
	rm -rf *-build *build.tmp*
