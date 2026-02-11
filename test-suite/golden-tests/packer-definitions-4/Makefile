all:
	morloc make main.loc 2> build.err
	./nexus foo 42 > obs.txt 2> obs.err

.PHONY: clean
clean:
	rm -rf pool* nexus* *pdf *rda __pycache__
