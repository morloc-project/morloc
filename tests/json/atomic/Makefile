TARGET=x.loc

all:
	rm -f manifold-nexus.py
	rm -rf locout
	loc -kx locout ${TARGET}
	> obs
	./manifold-nexus.py int_py >> obs
	./manifold-nexus.py str_py >> obs
	./manifold-nexus.py int_sh >> obs
	./manifold-nexus.py str_sh >> obs
	./manifold-nexus.py int_R >> obs
	./manifold-nexus.py str_R >> obs
	diff obs exp

.PHONY: clean
clean:
	rm -f manifold-nexus.py
	rm -rf locout
	rm -f obs
