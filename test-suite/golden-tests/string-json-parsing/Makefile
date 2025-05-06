all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus foo a.json > obs.txt

build:
	gcc -g -o nexus -I/home/z/.morloc/include nexus.c

clean:
	rm -rf nexus* pool* __pycache__
