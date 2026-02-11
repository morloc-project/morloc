all:
	rm -f obs.txt
	morloc make main.loc 2> build.err
	./nexus analyze --sample 1 > obs.txt 2> obs.err
	./nexus topWords --sample 1 2>> obs.err  >> obs.txt
	./nexus sentiment --sample 2 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
