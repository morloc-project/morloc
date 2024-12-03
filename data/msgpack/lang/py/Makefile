# This is a self-deleting Makefile. On success, only the .so file remains.

all:
	python setup.py build_ext --inplace
	cp -s pympack.cpython* pympack
	rm -rf build pympack.c setup.py Makefile
