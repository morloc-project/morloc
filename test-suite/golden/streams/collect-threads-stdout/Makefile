all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	for c in threadsPy threadsCpp ; do for i in 1 2 3 ; do ./nexus $$c 2>> obs.err | python3 check.py >> obs.txt ; done ; done

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
