# This is just a convenience Makefile for casual testing

all:
	stack install
	rm -f nexus.perl pool.R
	morloc make sample.loc http://localhost:3030/morloc

.PHONY: clean
clean:
	rm -rf nexus.* pool.* __pycache__ 

.PHONY: runall
runall:
	./nexus.perl
	./nexus.perl ceiling 1.1
	./nexus.perl test 10
	./nexus.perl rand_uniform 10 4 5
	./nexus.perl sum [1,2,3]
	./nexus.perl transpose [[1,2,3],[3,4,5]]
	./nexus.perl nrow '{"x":[1,2,3],"y":[2,3,4]}'
	./nexus.perl len [1,2,3,4] 
	./nexus.perl roll 10 4
