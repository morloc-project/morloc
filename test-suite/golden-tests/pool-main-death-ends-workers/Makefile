all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus orphanPy 20 > /dev/null 2> obs.py.err && echo UNEXPECTED_SUCCESS >> obs.txt || echo py_failed >> obs.txt
	./nexus orphanR 20 > /dev/null 2> obs.r.err && echo UNEXPECTED_SUCCESS >> obs.txt || echo r_failed >> obs.txt

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
