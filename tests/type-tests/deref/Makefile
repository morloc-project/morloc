TARGET=x.loc

all:
	loc -tkx locout ${TARGET}

.PHONY: run
run:
	./manifold-nexus.py main
	# ./manifold-nexus.py py
	# ./manifold-nexus.py R
	# ./manifold-nexus.py sh

.PHONY: clean
clean:
	rm -f manifold-nexus.py
	rm -rf locout
