all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	MORLOC_POOL_IDLE_TIMEOUT_MS=0 timeout 120 ./nexus nested 300 > obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err __pycache__
	rm -rf *-build *build.tmp*
