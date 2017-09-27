all:
	cabal install --enable-tests

.PHONY: test
test:
	cabal test

.PHONY: run
run:
	cabal run

.PHONY: clean
clean:
	rm -f nexus.sh pool.R
