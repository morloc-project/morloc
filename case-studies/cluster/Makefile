TARGET=cluster

all:
	morloc -kx locout ${TARGET}.loc

.PHONY: run
run:
	./manifold-nexus.py main

.PHONY: clean
clean:
	rm -f manifold-nexus.py *.pdf
	rm -rf locout
