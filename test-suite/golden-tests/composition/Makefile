all:
	rm -f obs.txt
	morloc make main.loc 
	./nexus foo '["x","y"]' > obs.txt
	./nexus bar 0 >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
