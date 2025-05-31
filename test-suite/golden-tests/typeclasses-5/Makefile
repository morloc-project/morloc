all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus square 4.0 > obs.txt
	./nexus sumOfSquares [1,2,3] >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
