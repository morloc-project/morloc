all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus foo 23 45 >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
