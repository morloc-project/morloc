export MORLOC_WRITE_BUFFER_BYTES = 4096

all:
	rm -f *.err obs.txt *.dat *.json
	morloc make -o nexus main.loc 2> build.err
	python3 gen.py
	test $$(grep -c 'morloc.mlc_write(' nexus-build/pools/py/pool.py) -eq $$(( 2 * $$(wc -l < types.txt) + 5 )) && echo py-writes >> obs.txt || echo UNEXPECTED_PY_WRITES >> obs.txt
	test $$(grep -c '^ *_mlc_write(mlc_schema' nexus-build/pools/cpp/pool.cpp) -eq $$(( 2 * $$(wc -l < types.txt) )) && echo cpp-writes >> obs.txt || echo UNEXPECTED_CPP_WRITES >> obs.txt
	test $$(grep -c 'rustmorloc::write(' nexus-build/pools/rust/src/main.rs) -eq $$(( 2 * $$(wc -l < types.txt) + 2 )) && echo rust-writes >> obs.txt || echo UNEXPECTED_RUST_WRITES >> obs.txt
	for t in $$(cat types.txt); do for z in 0 3; do \
	  for l in c r p; do \
	    ./nexus w$$l$$t $$z $$l.dat $$t.json 2>> obs.err || echo "FAIL w$$l$$t" >> obs.txt; \
	    MORLOC_STDOUT=stage ./nexus -f packet s$$l$$t $$z $$t.json > s$$l.dat 2>> obs.err || echo "FAIL s$$l$$t" >> obs.txt; \
	  done; \
	  echo "$$t $$z cpp $$(python3 same.py c.dat sc.dat) rust $$(python3 same.py r.dat sr.dat) py $$(python3 same.py p.dat sp.dat) cross $$(python3 same.py c.dat p.dat)" >> obs.txt; \
	done; done
	for z in 0 3; do \
	  for l in r p; do \
	    ./nexus w$${l}Wide $$z $$l.dat Wide.json 2>> obs.err || echo "FAIL w$${l}Wide" >> obs.txt; \
	    MORLOC_STDOUT=stage ./nexus -f packet s$${l}Wide $$z Wide.json > s$$l.dat 2>> obs.err || echo "FAIL s$${l}Wide" >> obs.txt; \
	  done; \
	  echo "Wide $$z rust $$(python3 same.py r.dat sr.dat) py $$(python3 same.py p.dat sp.dat) cross $$(python3 same.py r.dat p.dat)" >> obs.txt; \
	  ./nexus wpBig $$z p.dat Big.json 2>> obs.err || echo "FAIL wpBig" >> obs.txt; \
	  MORLOC_STDOUT=stage ./nexus -f packet spBig $$z Big.json > sp.dat 2>> obs.err || echo "FAIL spBig" >> obs.txt; \
	  echo "Big $$z py $$(python3 same.py p.dat sp.dat)" >> obs.txt; \
	  ./nexus wpBigOne $$z p.dat BigOne.json 2>> obs.err || echo "FAIL wpBigOne" >> obs.txt; \
	  ./nexus wnBigOne $$z n.dat BigOne.json 2>> obs.err || echo "FAIL wnBigOne" >> obs.txt; \
	  echo "BigOne $$z nexus $$(python3 same.py p.dat n.dat)" >> obs.txt; \
	done
	for t in $$(cat stdout-types.txt); do \
	  for l in c r p; do \
	    env -u MORLOC_WRITE_BUFFER_BYTES ./nexus w$$l$$t 3 $$l.dat $$t.json 2>> obs.err || echo "FAIL default w$$l$$t" >> obs.txt; \
	    env -u MORLOC_WRITE_BUFFER_BYTES MORLOC_STDOUT=stage ./nexus -f packet s$$l$$t 3 $$t.json > s$$l.dat 2>> obs.err || echo "FAIL default s$$l$$t" >> obs.txt; \
	  done; \
	  echo "default-buffer $$t cpp $$(python3 same.py c.dat sc.dat) rust $$(python3 same.py r.dat sr.dat) py $$(python3 same.py p.dat sp.dat) cross $$(python3 same.py c.dat p.dat)" >> obs.txt; \
	done

clean:
	rm -rf nexus pools __pycache__ *.err *.dat *.json
	rm -rf *-build *build.tmp*
