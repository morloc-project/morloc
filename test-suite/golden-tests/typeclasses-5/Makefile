all:
	rm -f obs.txt
	morloc make main.loc 2> build.err
	./nexus square 4.0 > obs.txt 2> obs.err
	./nexus sumOfSquares [1,2,3] 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
