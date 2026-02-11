all:
	rm -f obs.txt
	morloc make foo.loc 2> build.err
	>obs.txt
	./nexus foop a.json 2>> obs.err  >> obs.txt
	./nexus foor a.json 2>> obs.err  >> obs.txt
	./nexus fooc a.json 2>> obs.err  >> obs.txt
	./nexus foop b.json 2>> obs.err  >> obs.txt
	./nexus foor b.json 2>> obs.err  >> obs.txt
	./nexus fooc b.json 2>> obs.err  >> obs.txt

build:
	gcc -g -o nexus -I/home/z/.morloc/include nexus.c

clean:
	rm -rf nexus* pool* __pycache__
