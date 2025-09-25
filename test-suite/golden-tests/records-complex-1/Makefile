all:
	rm -f obs.txt
	morloc make foo.loc > log
	./nexus foo 42 > obs.txt
	./nexus bar 42 >> obs.txt

build:
	gcc -o nexus -O -I/home/z/.morloc/include nexus.c
	g++ -O3 --std=c++17 -o pool-cpp.out pool.cpp -std=c++17 -I/home/z/.morloc/include -I/home/z/.morloc/src/mor

clean:
	rm -rf pool* nexus* __pycache__
