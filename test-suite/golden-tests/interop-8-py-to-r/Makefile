all:
	rm -f obs.txt
	morloc make main.loc 
	./nexus foo "true" > obs.txt
	diff exp.txt obs.txt

clean:
	rm -rf nexus* pool* __pycache__
