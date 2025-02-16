all:
	morloc make foo.loc
	./nexus.py dobytes '"asdf"' > obs.txt
	./nexus.py doarray '"asdf"' >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
