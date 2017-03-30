TARGET=x.loc

all:
	morloc -kx locout ${TARGET}

.PHONY: run
run:
	./manifold-nexus.py say_hi
	./manifold-nexus.py say_bi

.PHONY: clean
clean:
	rm -f manifold-nexus.py
	rm -rf locout
