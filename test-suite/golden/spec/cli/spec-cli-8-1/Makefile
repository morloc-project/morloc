all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	morloc make -o nexus-py main-py.loc 2>> build.err
	./nexus pieces 1 >> obs.txt 2>> obs.err
	./nexus pieces --whole 1 >> obs.txt 2>> obs.err
	./nexus -z 3 pieces 1 >> obs.txt 2>> obs.err
	./nexus bytes 1 >> obs.txt 2>> obs.err
	./nexus bytes --whole 1 >> obs.txt 2>> obs.err
	./nexus -z 3 bytes 1 >> obs.txt 2>> obs.err
	./nexus big 1 > big.out 2>> obs.err
	python3 expected.py | cmp -s - big.out && echo BIG-SAME >> obs.txt || echo BIG-DIFFERS >> obs.txt
	./nexus-py pieces 1 >> obs.txt 2>> obs.err
	./nexus-py pieces --whole 1 >> obs.txt 2>> obs.err
	./nexus-py -z 3 pieces 1 >> obs.txt 2>> obs.err
	./nexus-py bytes 1 >> obs.txt 2>> obs.err
	./nexus-py bytes --whole 1 >> obs.txt 2>> obs.err
	./nexus-py -z 3 bytes 1 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus nexus-py pools __pycache__ *.err *.out
	rm -rf *-build *build.tmp*
