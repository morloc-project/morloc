all:
	morloc make -o nexus-py main-py.loc 2> build.err
	morloc make -o nexus-r main-r.loc 2> build.err
	morloc make -o nexus-cpp main-cpp.loc 2> build.err
	./nexus-py test > obs.txt 2> obs.err
	./nexus-r test 2>> obs.err  >> obs.txt
	./nexus-cpp test 2>> obs.err  >> obs.txt

build:
	g++ -O3 --std=c++17 -o pool-cpp.out pool.cpp -std=c++17 -I/home/z/.local/share/morloc/include -I/home/z/.local/share/morloc/src/morloc/plane/default/root-cpp -I.

clean:
	rm -rf nexus* pool* __pycache__
