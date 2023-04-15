all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus.py foo '["x","y"]' > obs.txt
	./nexus.py bar 0 >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
