all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus foo 5 7 > obs.txt

clean:
	rm -rf nexus* pool* __pycache__
