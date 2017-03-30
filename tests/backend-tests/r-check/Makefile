TARGET=x.loc

all:
	morloc -tkx locout ${TARGET}

.PHONY: run
run:
	./manifold-nexus.py sqrt

.PHONY: clean
clean:
	rm -f manifold-nexus.py
	rm -rf locout
