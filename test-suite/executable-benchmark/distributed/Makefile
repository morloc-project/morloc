all:
	morloc make main.loc
	./nexus bar [1,2,3]

build:
	gcc -g -o nexus -O -I/home/z/.morloc/include nexus.c
	g++ -g --std=c++17 -o pool-cpp.out pool.cpp -std=c++17 -I/home/z/.morloc/include -I.

clean:
	rm -rf __pycache__ pool* nexus* log
