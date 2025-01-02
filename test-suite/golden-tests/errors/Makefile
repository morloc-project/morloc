all:
	rm -f obs.txt
	morloc make main.loc > log
	./nexus.py ccerr 0 2>&1 | grep -c "zero" >  obs.txt || true
	./nexus.py cperr 0 2>&1 | grep -c "zero" >> obs.txt || true
	./nexus.py crerr 0 2>&1 | grep -c "zero" >> obs.txt || true
	./nexus.py pcerr 0 2>&1 | grep -c "zero" >> obs.txt || true
	./nexus.py pperr 0 2>&1 | grep -c "zero" >> obs.txt || true
	./nexus.py prerr 0 2>&1 | grep -c "zero" >> obs.txt || true
	./nexus.py rcerr 0 2>&1 | grep -c "zero" >> obs.txt || true
	./nexus.py rperr 0 2>&1 | grep -c "zero" >> obs.txt || true
	./nexus.py rrerr 0 2>&1 | grep -c "zero" >> obs.txt || true

clean:
	rm -rf nexus.py pool-cpp.out __pycache__
