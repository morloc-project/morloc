# This is a self-deleting Makefile. On success, only the .so file remains.

PYTHON=python3

all:
	${PYTHON} setup.py build_ext --inplace
	cp -fs pymorloc.cpython* pymorloc
	rm -rf build pymorloc.c setup.py makefile
