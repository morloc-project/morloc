all:
	rm -f obs.txt
	morloc make main.loc 2> build.err
	./nexus foo '["x","y"]' > obs.txt 2> obs.err
	./nexus bar 0 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
