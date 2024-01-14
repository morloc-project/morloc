all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	echo '"a"' > a.json
	echo '"b"' > b.json
	>obs.txt
	./nexus.py foo a.json b.json >> obs.txt
	./nexus.py foo '"a"' b.json >> obs.txt
	./nexus.py foo '"a"' '"b"' >> obs.txt
	./nexus.py foo '"a"' <(echo '"b"') >> obs.txt
	echo '"b"' | ./nexus.py foo '"a"' /dev/stdin >> obs.txt

build:
	g++ --std=c++17 -o pool-cpp.out pool.cpp -I/home/z/.morloc/include -I.

clean:
	rm -rf nexus* pool* __pycache__ a.json b.json
