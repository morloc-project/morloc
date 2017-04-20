TARGET=x.loc

all:
	morloc -kx locout ${TARGET}

.PHONY: run
run:
	./manifold-nexus.py main

.PHONY: clean
clean:
	rm -f manifold-nexus.py
	rm -rf locout
