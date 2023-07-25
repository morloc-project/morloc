all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus.py foo "true" > obs.txt
	diff exp.txt obs.txt

clean:
	rm -rf nexus* pool* __pycache__
