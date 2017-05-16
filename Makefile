all:
	cabal install --enable-tests

.PHONY: test
test:
	cabal test
