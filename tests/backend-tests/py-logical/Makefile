TARGET=x.loc

all:
	morloc -kx locout ${TARGET}

.PHONY: run
run:
	./manifold-nexus.py a
	./manifold-nexus.py b

.PHONY: clean
clean:
	rm -f manifold-nexus.py
	rm -rf locout
