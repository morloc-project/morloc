all:
	morloc make -o nexus-py main-py.loc > log
	morloc make -o nexus-r main-r.loc > log
	morloc make -o nexus-cpp main-cpp.loc > log
	./nexus-py test > obs.txt
	./nexus-r test >> obs.txt
	./nexus-cpp test >> obs.txt

build:
	g++ -O3 --std=c++17 -o pool-cpp.out pool.cpp -std=c++17 -I/home/z/.local/share/morloc/include -I/home/z/.local/share/morloc/src/morloc/plane/default/root-cpp -I.

clean:
	rm -rf nexus* pool* __pycache__
