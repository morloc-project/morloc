all:
	rm -f obs.txt *.err
	morloc make main.loc 2> build.err
	./nexus foo '"asdf"' 3.14 > obs.txt 2> obs.err

.PHONY: clean
clean:
	rm -rf pool* nexus* __pycache__
