all:
	rm -f obs.txt
	morloc make main.loc 2> build.err
	./nexus foo 37 > obs.txt 2> obs.err

clean:
	rm -rf pool* nexus* __pycache__
