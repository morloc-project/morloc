all:
	rm -f obs.txt
	morloc make foo.loc 
	>obs.txt
	./nexus foop a.json >> obs.txt
	./nexus foor a.json >> obs.txt
	./nexus fooc a.json >> obs.txt
	./nexus foop b.json >> obs.txt
	./nexus foor b.json >> obs.txt
	./nexus fooc b.json >> obs.txt

build:
	gcc -g -o nexus -I/home/z/.morloc/include nexus.c

clean:
	rm -rf nexus* pool* __pycache__
