all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err && echo "render-record-arg-build:ok" >> obs.txt || echo "render-record-arg-build:FAILED" >> obs.txt

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
