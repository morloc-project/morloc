export MORLOC_WRITE_BUFFER_BYTES = 4096
COPY = morloc-nexus view -p

all:
	rm -f *.err obs.txt *.dat *.json
	morloc make -o nexus main.loc 2> build.err
	python3 gen.py
	test $$(sed -n '/AUTO include manifolds start/,/AUTO include manifolds end/p' nexus-build/pools/py/pool.py | grep -c 'morloc.mlc_write(') -eq $$(( 2 * $$(wc -l < types.txt) + 5 )) && echo py-writes >> obs.txt || echo UNEXPECTED_PY_WRITES >> obs.txt
	test $$(grep -c '^ *_mlc_write(mlc_schema' nexus-build/pools/cpp/pool.cpp) -eq $$(( 2 * $$(wc -l < types.txt) )) && echo cpp-writes >> obs.txt || echo UNEXPECTED_CPP_WRITES >> obs.txt
	test $$(grep -c 'rustmorloc::write(' nexus-build/pools/rust/src/main.rs) -eq $$(( 2 * $$(wc -l < types.txt) + 2 )) && echo rust-writes >> obs.txt || echo UNEXPECTED_RUST_WRITES >> obs.txt
	test $$(grep -F -c 'morloc$$morloc_mlc_write(' nexus-build/pools/r/pool.R) -eq $$(( 2 * $$(wc -l < types-r.txt) + $$(wc -l < types-rl.txt) )) && echo r-writes >> obs.txt || echo UNEXPECTED_R_WRITES >> obs.txt
	for t in $$(cat types.txt); do for z in 0 3; do \
	  for l in c r p; do \
	    ./nexus w$$l$$t $$z $$l.dat $$t.json 2>> obs.err || echo "FAIL w$$l$$t" >> obs.txt; \
	    ./nexus -f packet s$$l$$t $$z $$t.json > s$$l.dat 2>> obs.err || echo "FAIL s$$l$$t" >> obs.txt; \
	    $(COPY) -z $$z $$l.dat -o v$$l.dat 2>> obs.err || echo "FAIL copy $$l$$t" >> obs.txt; \
	  done; \
	  echo "$$t $$z cpp $$(python3 same.py c.dat sc.dat) $$(python3 same.py c.dat vc.dat) rust $$(python3 same.py r.dat sr.dat) $$(python3 same.py r.dat vr.dat) py $$(python3 same.py p.dat sp.dat) $$(python3 same.py p.dat vp.dat) cross $$(python3 same.py c.dat p.dat)" >> obs.txt; \
	done; done
	for z in 0 3; do \
	  for l in r p; do \
	    ./nexus w$${l}Wide $$z $$l.dat Wide.json 2>> obs.err || echo "FAIL w$${l}Wide" >> obs.txt; \
	    ./nexus -f packet s$${l}Wide $$z Wide.json > s$$l.dat 2>> obs.err || echo "FAIL s$${l}Wide" >> obs.txt; \
	    $(COPY) -z $$z $$l.dat -o v$$l.dat 2>> obs.err || echo "FAIL copy $${l}Wide" >> obs.txt; \
	  done; \
	  echo "Wide $$z rust $$(python3 same.py r.dat sr.dat) $$(python3 same.py r.dat vr.dat) py $$(python3 same.py p.dat sp.dat) $$(python3 same.py p.dat vp.dat) cross $$(python3 same.py r.dat p.dat)" >> obs.txt; \
	  ./nexus wpBig $$z p.dat Big.json 2>> obs.err || echo "FAIL wpBig" >> obs.txt; \
	  ./nexus -f packet spBig $$z Big.json > sp.dat 2>> obs.err || echo "FAIL spBig" >> obs.txt; \
	  $(COPY) -z $$z p.dat -o vp.dat 2>> obs.err || echo "FAIL copy pBig" >> obs.txt; \
	  echo "Big $$z py $$(python3 same.py p.dat sp.dat) $$(python3 same.py p.dat vp.dat)" >> obs.txt; \
	  ./nexus wpBigOne $$z p.dat BigOne.json 2>> obs.err || echo "FAIL wpBigOne" >> obs.txt; \
	  ./nexus wnBigOne $$z n.dat BigOne.json 2>> obs.err || echo "FAIL wnBigOne" >> obs.txt; \
	  echo "BigOne $$z nexus $$(python3 same.py p.dat n.dat)" >> obs.txt; \
	done
	for t in $$(cat types-r.txt); do python3 rsafe.py $$t.json r.json; for z in 0 3; do \
	  ./nexus wp$$t $$z p.dat r.json 2>> obs.err || echo "FAIL wp$$t" >> obs.txt; \
	  ./nexus wv$$t $$z v.dat r.json 2>> obs.err || echo "FAIL wv$$t" >> obs.txt; \
	  ./nexus -f packet sv$$t $$z r.json > sv.dat 2>> obs.err || echo "FAIL sv$$t" >> obs.txt; \
	  $(COPY) -z $$z v.dat -o vv.dat 2>> obs.err || echo "FAIL copy v$$t" >> obs.txt; \
	  echo "$$t $$z r-lang $$(python3 same.py v.dat sv.dat) $$(python3 same.py v.dat vv.dat) cross $$(python3 same.py v.dat p.dat)" >> obs.txt; \
	done; done
	for t in $$(cat types-rl.txt); do python3 rsafe.py $$t.json r.json; for z in 0 3; do \
	  ./nexus wp$$t $$z p.dat r.json 2>> obs.err || echo "FAIL wp$$t" >> obs.txt; \
	  ./nexus wl$$t $$z v.dat r.json 2>> obs.err || echo "FAIL wl$$t" >> obs.txt; \
	  $(COPY) -z $$z v.dat -o vv.dat 2>> obs.err || echo "FAIL copy l$$t" >> obs.txt; \
	  echo "$$t $$z r-list $$(python3 same.py v.dat vv.dat) cross $$(python3 same.py v.dat p.dat)" >> obs.txt; \
	done; done
	for t in $$(cat stdout-types.txt); do \
	  for l in c r p; do \
	    env -u MORLOC_WRITE_BUFFER_BYTES ./nexus w$$l$$t 3 $$l.dat $$t.json 2>> obs.err || echo "FAIL default w$$l$$t" >> obs.txt; \
	    env -u MORLOC_WRITE_BUFFER_BYTES ./nexus -f packet s$$l$$t 3 $$t.json > s$$l.dat 2>> obs.err || echo "FAIL default s$$l$$t" >> obs.txt; \
	    env -u MORLOC_WRITE_BUFFER_BYTES $(COPY) -z 3 $$l.dat -o v$$l.dat 2>> obs.err || echo "FAIL default copy $$l$$t" >> obs.txt; \
	  done; \
	  echo "default-buffer $$t cpp $$(python3 same.py c.dat sc.dat) $$(python3 same.py c.dat vc.dat) rust $$(python3 same.py r.dat sr.dat) $$(python3 same.py r.dat vr.dat) py $$(python3 same.py p.dat sp.dat) $$(python3 same.py p.dat vp.dat) cross $$(python3 same.py c.dat p.dat)" >> obs.txt; \
	done

clean:
	rm -rf nexus pools __pycache__ *.err *.dat *.json
	rm -rf *-build *build.tmp*
