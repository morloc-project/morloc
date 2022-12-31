all:
	rm -f obs.txt
	morloc make main.loc
	./nexus.pl foo '["x","y"]' > obs.txt
	./nexus.pl bar 0 >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
