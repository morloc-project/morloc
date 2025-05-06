all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus pfoo a.json > obs.txt
	./nexus pfoo b.json >> obs.txt
	./nexus cfoo a.json >> obs.txt
	./nexus cfoo b.json >> obs.txt
	./nexus rfoo a.json >> obs.txt
	./nexus rfoo b.json >> obs.txt

build:
	gcc -g -o nexus -I/home/z/.morloc/include nexus.c

clean:
	rm -rf nexus* pool* __pycache__
