all:
	rm -f obs.txt
	morloc make foo.loc 2> build.err
	# g++ -O3 --std=c++17 -o pool-cpp.out pool.cpp -std=c++17 -I/home/z/.morloc/include -I.
	./nexus foo [7,8,9] > obs.txt 2> obs.err

clean:
	rm -rf nexus* pool* __pycache__
