all:
	rm -f obs.txt
	morloc make foo.loc 2> build.err
	./nexus pfoo a.json > obs.txt 2> obs.err
	./nexus pfoo b.json 2>> obs.err  >> obs.txt
	./nexus cfoo a.json 2>> obs.err  >> obs.txt
	./nexus cfoo b.json 2>> obs.err  >> obs.txt
	./nexus rfoo a.json 2>> obs.err  >> obs.txt
	./nexus rfoo b.json 2>> obs.err  >> obs.txt

build:
	gcc -g -o nexus -I/home/z/.morloc/include nexus.c

clean:
	rm -rf nexus* pool* __pycache__
