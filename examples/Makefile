# This is just a convenience Makefile for casual testing

all:
	stack build --test --fast --haddock-deps
	stack install
	rm -f nexus.pl pool.R
	morloc make mathy-math.loc
	perl nexus.pl -h
	perl nexus.pl meanfib 10
	morloc make deep.loc
	perl nexus.pl -h
	perl nexus.pl rollsum 500
	morloc make sample-complex.loc
	perl nexus.pl -h
	perl nexus.pl test 5
	morloc make arithmetic.loc
	perl nexus.pl -h
	perl nexus.pl rms [1,2,3]

.PHONY: clean
clean:
	rm -rf nexus.* pool* __pycache__ 
	rm -rf z.ttl z.rq
