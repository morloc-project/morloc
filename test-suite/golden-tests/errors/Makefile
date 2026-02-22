all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus ccerr 0 2>&1 | grep -c "zero" >  obs.txt || true
	./nexus cperr 0 2>&1 | grep -c "zero" 2>> obs.err  >> obs.txt || true
	./nexus crerr 0 2>&1 | grep -c "zero" 2>> obs.err  >> obs.txt || true
	./nexus pcerr 0 2>&1 | grep -c "zero" 2>> obs.err  >> obs.txt || true
	./nexus pperr 0 2>&1 | grep -c "zero" 2>> obs.err  >> obs.txt || true
	./nexus prerr 0 2>&1 | grep -c "zero" 2>> obs.err  >> obs.txt || true
	./nexus rcerr 0 2>&1 | grep -c "zero" 2>> obs.err  >> obs.txt || true
	./nexus rperr 0 2>&1 | grep -c "zero" 2>> obs.err  >> obs.txt || true
	./nexus rrerr 0 2>&1 | grep -c "zero" 2>> obs.err  >> obs.txt || true

clean:
	rm -rf nexus poolscpp.out __pycache__
