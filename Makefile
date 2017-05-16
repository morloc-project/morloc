all:
	cabal install --enable-tests

.PHONY: test
test:
	cabal test

.PHONY: run
run:
	cabal run
