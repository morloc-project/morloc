all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus foo 2 > obs.txt

build:
	gcc -g -o nexus -I/home/z/.morloc/include nexus.c
	g++ -g -o pool-cpp.out pool.cpp -std=c++17 -I/home/z/.morloc/include -I/home/z/.morloc/src/morloc/plane/morloclib/cppbase


clean:
	rm -f nexus* pool*
