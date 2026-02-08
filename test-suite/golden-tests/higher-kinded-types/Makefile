all:
	morloc make foo.loc 
	>obs.txt
	./nexus foo1 12 >> obs.txt
	./nexus foo2 12 >> obs.txt
	./nexus foo3 12 >> obs.txt
	./nexus foo4 12 >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
