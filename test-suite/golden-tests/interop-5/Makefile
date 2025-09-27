all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus foo [1,2,3] > obs.txt

clean:
	rm -rf nexus* pool* *pdf __pycache__
