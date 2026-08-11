all:
	timeout 60 morloc make -o nexus main.loc 2> build.err && echo OK > obs.txt || echo TIMEOUT_OR_FAIL > obs.txt

clean:
	rm -rf nexus pools *.err __pycache__ obs.txt.tmp
