all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus pingPong 7 >> obs.txt 2>> obs.err
	timeout 120 ./nexus pingPong 10000 >> obs.txt 2>> obs.err || echo "pingPong 10000 failed (rc=$$?)" >> obs.txt
	./nexus threeWay 7 >> obs.txt 2>> obs.err
	timeout 120 ./nexus threeWay 10000 >> obs.txt 2>> obs.err || echo "threeWay 10000 failed (rc=$$?)" >> obs.txt

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
