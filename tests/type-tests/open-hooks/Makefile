TARGET=x.loc

all:
	morloc -kx locout ${TARGET}

.PHONY: run
run:
	./manifold-nexus.py py
	./manifold-nexus.py r
	./manifold-nexus.py sh

.PHONY: clean
clean:
	rm -f manifold-nexus.py
	rm -rf locout
