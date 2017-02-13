TARGET=x.loc

all:
	loc -kx locout ${TARGET}

.PHONY: run
run:
	./manifold-nexus.py foo

.PHONY: clean
clean:
	rm -f manifold-nexus.py
	rm -rf locout
