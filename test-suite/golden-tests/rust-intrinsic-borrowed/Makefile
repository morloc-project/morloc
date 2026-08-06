all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus demo [1,2] [3,4] >> obs.txt 2>> obs.err
	./nexus demo [10,20,30] [40] >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
