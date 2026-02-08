all:
	rm -f obs.txt
	morloc make main.loc
	./nexus analyze -n 50 > obs.txt
	./nexus summary >> obs.txt
	./nexus groupBy -n 50 >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
