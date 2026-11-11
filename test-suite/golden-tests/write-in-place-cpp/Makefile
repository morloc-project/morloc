export MORLOC_WRITE_BUFFER_BYTES = 4096

all:
	rm -f *.err obs.txt *.dat *.json
	morloc make -o nexus main.loc 2> build.err
	python3 gen.py
	test $$(grep -c 'morloc.mlc_write(' nexus-build/pools/py/pool.py) -eq $$(wc -l < types.txt) && echo py-writes >> obs.txt || echo UNEXPECTED_PY_WRITES >> obs.txt
	for t in $$(cat types.txt); do for z in 0 3; do \
	  ./nexus wc$$t $$z c.dat $$t.json 2>> obs.err; \
	  ./nexus wp$$t $$z p.dat $$t.json 2>> obs.err; \
	  echo "$$t $$z $$(python3 same.py c.dat p.dat)" >> obs.txt; \
	done; done

clean:
	rm -rf nexus pools __pycache__ *.err *.dat *.json
	rm -rf *-build *build.tmp*
