# This is just a convenience Makefile for casual testing

all:
	stack install
	rm -f nexus.perl pool.R
	morloc make sample.loc http://localhost:3030/morloc

.PHONY: clean
clean:
	rm -f nexus.* pool.*
