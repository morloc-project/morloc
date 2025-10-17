all:
	morloc make main.loc > log
	./nexus foo > obs.txt
	./nexus bar >> obs.txt
	./nexus baz >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
