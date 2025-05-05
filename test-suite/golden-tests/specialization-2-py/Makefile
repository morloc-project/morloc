all:
	morloc make foo.loc
	./nexus dobytes '"asdf"' > obs.txt
	./nexus doarray '"asdf"' >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
