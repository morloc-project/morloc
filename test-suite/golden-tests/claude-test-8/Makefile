all:
	rm -f obs.txt
	morloc make main.loc
	./nexus analyze --sample 1 > obs.txt
	./nexus topWords --sample 1 >> obs.txt
	./nexus sentiment --sample 2 >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
