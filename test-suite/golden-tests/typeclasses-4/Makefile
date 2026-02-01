all:
	rm -f obs.txt
	morloc make main.loc 
	./nexus foo '["a","bad"]' > obs.txt

clean:
	rm -rf nexus* pool* __pycache__
