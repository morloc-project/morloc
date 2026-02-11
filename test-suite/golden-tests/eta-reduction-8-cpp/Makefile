all:
	rm -f obs.txt
	morloc make main.loc 2> build.err
	./nexus foo 23 35 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
