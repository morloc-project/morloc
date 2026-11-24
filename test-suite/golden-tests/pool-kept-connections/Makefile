all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	timeout 120 ./nexus cppToPy 2000 >> obs.txt 2>> obs.err
	timeout 120 ./nexus pyToR 1000 >> obs.txt 2>> obs.err
	MORLOC_POOL_IDLE_TIMEOUT_MS=0 timeout 120 ./nexus nested 200 >> obs.txt 2>> obs.err
	MORLOC_POOL_IDLE_TIMEOUT_MS=0 timeout 120 ./nexus parallel 400 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
