all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus foo '["a","bad"]' > obs.txt

clean:
	rm -rf nexus* pool* __pycache__
