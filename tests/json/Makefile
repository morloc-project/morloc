TARGET=x.loc

all:
	loc -kx locout ${TARGET}

.PHONY: run
run:
	./manifold-nexus.py int_py
	./manifold-nexus.py str_py
	./manifold-nexus.py arr_py
	# ./manifold-nexus.py int_sh
	# ./manifold-nexus.py str_sh
	# ./manifold-nexus.py int_R
	# ./manifold-nexus.py str_R

.PHONY: clean
clean:
	rm -f manifold-nexus.py
	rm -rf locout
