all:
	morloc make foo.loc
	./nexus srcFoo '"c"' > obs.txt
	./nexus natFoo '"c"' >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
