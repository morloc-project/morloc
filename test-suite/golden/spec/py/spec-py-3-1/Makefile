SRCS = bump.py copy.py app/main.py
PYC = python3 -I -c 'import py_compile as c, sys; [c.compile(p, invalidation_mode=c.PycInvalidationMode.TIMESTAMP, doraise=True) for p in sys.argv[1:]]'

all:
	rm -rf *.err obs.txt app __pycache__ *.ref bump.py copy.py bumper.py
	mkdir app
	touch app/__init__.py
	printf 'def bump(x):\n    return x + 100.0\n' > bump.py
	printf 'def copy_bump(x):\n    return x + 100.0\n' > copy.py
	printf 'def app_bump(x):\n    return x + 100.0\n' > app/main.py
	printf 'x = 1\n' > bumper.py
	for f in $(SRCS); do cp -p $$f $$(echo $$f | tr / _).ref; done
	$(PYC) $(SRCS) bumper.py
	morloc make -o nexus main.loc 2> build.err
	./nexus bumpIt 1 > obs.txt 2>> obs.err
	./nexus copyIt 1 >> obs.txt 2>> obs.err
	./nexus appIt 1 >> obs.txt 2>> obs.err
	$(PYC) $(SRCS)
	printf 'def bump(x):\n    return x + 200.0\n' > bump.py
	printf 'def copy_bump(x):\n    return x + 200.0\n' > copy.py
	printf 'def app_bump(x):\n    return x + 200.0\n' > app/main.py
	for f in $(SRCS); do touch -r $$(echo $$f | tr / _).ref $$f; done
	morloc make -o nexus main.loc 2>> build.err
	./nexus bumpIt 1 >> obs.txt 2>> obs.err
	./nexus copyIt 1 >> obs.txt 2>> obs.err
	./nexus appIt 1 >> obs.txt 2>> obs.err
	ls __pycache__ | grep -c '^bumper\.' >> obs.txt

clean:
	rm -rf nexus pools *-build *build.tmp* app __pycache__ *.ref bump.py copy.py bumper.py
