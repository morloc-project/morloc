TARGET=x.loc

all:
	morloc -kx locout ${TARGET}

.PHONY: run
run:
	./manifold-nexus.py R
	./manifold-nexus.py sh
	./manifold-nexus.py py

.PHONY: clean
clean:
	rm -f manifold-nexus.py
	rm -rf locout
