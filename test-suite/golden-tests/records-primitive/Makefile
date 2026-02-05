all:
	rm -f obs.txt
	morloc make foo.loc 
	./nexus foo 42 > obs.txt
	./nexus bar 42 >> obs.txt

build:
	gcc -o nexus -O -I/home/z/.local/share/morloc/include nexus.c
	g++ -O3 --std=c++17 -o pool-cpp.out pool.cpp -std=c++17 -I/home/z/.local/share/morloc/include -I.

clean:
	rm -rf pool* nexus* __pycache__
