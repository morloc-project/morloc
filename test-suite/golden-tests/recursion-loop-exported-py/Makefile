all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus sumTo 0 10 >> obs.txt 2>> obs.err
	./nexus sumTo 0 100000 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
