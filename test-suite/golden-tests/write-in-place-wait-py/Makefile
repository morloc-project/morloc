export MORLOC_WRITE_BUFFER_BYTES = 4096
export MORLOC_WRITE_BEHIND_DEPTH = 1

all:
	rm -f *.err obs.txt *.dat
	morloc make -o nexus main.loc 2> build.err
	test $$(grep -c 'morloc.mlc_write(' nexus-build/pools/py/pool.py) -eq 17 && echo py-writes >> obs.txt || echo UNEXPECTED_PY_WRITES >> obs.txt
	for z in 0 3; do \
	  for tk in Reals:np I32s:np I8s:np U8s:np U8s:bytes U8s:bytearray Bools:np Bools:bytes I16s:np I16s:np8; do \
	    t=$${tk%%:*}; k=$${tk#*:}; \
	    ./nexus w$$t list $$z l.dat 2>> obs.err || echo "FAIL w$$t list" >> obs.txt; \
	    ./nexus w$$t $$k $$z k.dat 2>> obs.err || echo "FAIL w$$t $$k" >> obs.txt; \
	    echo "$$t $$k $$z $$(python3 same.py l.dat k.dat)" >> obs.txt; \
	  done; \
	done
	for z in 0 3 9; do \
	  for t in Reals I32s Strs; do \
	    ./nexus w$$t list $$z f.dat 2>> obs.err || echo "FAIL w$$t list" >> obs.txt; \
	    MORLOC_STDOUT=stage ./nexus -f packet s$$t list $$z > s.dat 2>> obs.err || echo "FAIL s$$t list" >> obs.txt; \
	    echo "staged $$t $$z $$(python3 same.py f.dat s.dat)" >> obs.txt; \
	  done; \
	done
	./nexus badBools b.dat >> obs.txt 2>> obs.err
	./nexus countBools b.dat >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err *.dat
	rm -rf *-build *build.tmp*
