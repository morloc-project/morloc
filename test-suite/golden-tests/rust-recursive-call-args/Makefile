all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus sumTo 100 >> obs.txt 2>> obs.err
	./nexus down 10.0 >> obs.txt 2>> obs.err
	./nexus alt true 7 >> obs.txt 2>> obs.err
	./nexus grid 5 1 >> obs.txt 2>> obs.err
	./nexus sumDec 100 >> obs.txt 2>> obs.err
	./nexus countA 5 >> obs.txt 2>> obs.err
	./nexus addTwos 50 >> obs.txt 2>> obs.err
	./nexus ack 2 3 >> obs.txt 2>> obs.err
	./nexus grow x 3 >> obs.txt 2>> obs.err
	./nexus build [] 2 >> obs.txt 2>> obs.err
	./nexus compose 2 >> obs.txt 2>> obs.err
	./nexus adds 2 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
