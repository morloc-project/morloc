TARGET=x.loc

all:
	loc -kx locout ${TARGET}

.PHONY: run
run:
	./manifold-nexus.py Int
	./manifold-nexus.py Num
	./manifold-nexus.py String
	./manifold-nexus.py Bool

.PHONY: clean
clean:
	rm -f manifold-nexus.py
	rm -rf locout
