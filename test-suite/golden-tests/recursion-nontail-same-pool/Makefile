all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus total 200 >> obs.txt 2>> obs.err
	./nexus totalRust 200 >> obs.txt 2>> obs.err
	./nexus totalPy 200 >> obs.txt 2>> obs.err
	./nexus mutual 200 >> obs.txt 2>> obs.err
	./nexus carried 3 >> obs.txt 2>> obs.err
	RUST_MIN_STACK=67108864 timeout 60 ./nexus total 20000 >> obs.txt 2>> obs.err || echo "total 20000 failed" >> obs.txt

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
