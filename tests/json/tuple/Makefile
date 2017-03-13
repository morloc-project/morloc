TARGET=x.loc

all:
	rm -f manifold-nexus.py
	rm -rf locout
	loc -x locout ${TARGET}
	> obs
	./manifold-nexus.py py >> obs
	./manifold-nexus.py sh >> obs
	./manifold-nexus.py r >> obs
	diff obs exp

.PHONY: clean
clean:
	rm -f manifold-nexus.py
	rm -rf locout
	rm -f obs
