export MORLOC_WRITE_BUFFER_BYTES = 4096

all:
	rm -f *.err obs.txt *.dat *.json
	morloc make -o nexus main.loc 2> build.err
	python3 gen.py
	test $$(grep -c 'morloc.mlc_write(' nexus-build/pools/py/pool.py) -eq $$(cat types.txt stdout-types.txt | wc -l) && echo py-writes >> obs.txt || echo UNEXPECTED_PY_WRITES >> obs.txt
	test $$(grep -c '^ *_mlc_write(mlc_schema' nexus-build/pools/cpp/pool.cpp) -eq $$(cat types.txt stdout-types.txt | wc -l) && echo cpp-writes >> obs.txt || echo UNEXPECTED_CPP_WRITES >> obs.txt
	test $$(grep -c 'rustmorloc::write(' nexus-build/pools/rust/src/main.rs) -eq $$(cat types.txt stdout-types.txt | wc -l) && echo rust-writes >> obs.txt || echo UNEXPECTED_RUST_WRITES >> obs.txt
	for t in $$(cat types.txt); do for z in 0 3; do \
	  ./nexus wp$$t $$z p.dat $$t.json 2>> obs.err || echo "FAIL wp$$t" >> obs.txt; \
	  ./nexus wc$$t $$z c.dat $$t.json 2>> obs.err || echo "FAIL wc$$t" >> obs.txt; \
	  ./nexus wr$$t $$z r.dat $$t.json 2>> obs.err || echo "FAIL wr$$t" >> obs.txt; \
	  echo "$$t $$z cpp $$(python3 same.py c.dat p.dat) rust $$(python3 same.py r.dat p.dat)" >> obs.txt; \
	done; done
	for t in $$(cat stdout-types.txt); do \
	  ./nexus -f packet sp$$t 3 $$t.json > p.dat 2>> obs.err || echo "FAIL sp$$t" >> obs.txt; \
	  ./nexus -f packet sc$$t 3 $$t.json > c.dat 2>> obs.err || echo "FAIL sc$$t" >> obs.txt; \
	  ./nexus -f packet sr$$t 3 $$t.json > r.dat 2>> obs.err || echo "FAIL sr$$t" >> obs.txt; \
	  echo "stdout $$t cpp $$(python3 same.py c.dat p.dat) rust $$(python3 same.py r.dat p.dat)" >> obs.txt; \
	  env -u MORLOC_WRITE_BUFFER_BYTES ./nexus wp$$t 3 p.dat $$t.json 2>> obs.err || echo "FAIL default wp$$t" >> obs.txt; \
	  env -u MORLOC_WRITE_BUFFER_BYTES ./nexus wc$$t 3 c.dat $$t.json 2>> obs.err || echo "FAIL default wc$$t" >> obs.txt; \
	  env -u MORLOC_WRITE_BUFFER_BYTES ./nexus wr$$t 3 r.dat $$t.json 2>> obs.err || echo "FAIL default wr$$t" >> obs.txt; \
	  echo "default-buffer $$t cpp $$(python3 same.py c.dat p.dat) rust $$(python3 same.py r.dat p.dat)" >> obs.txt; \
	done

clean:
	rm -rf nexus pools __pycache__ *.err *.dat *.json
	rm -rf *-build *build.tmp*
