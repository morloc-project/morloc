all:
	rm -f *.err obs.txt *.err
	morloc make -o nexus foo.loc 2> build.err
	>obs.txt
	./nexus foo1 12 2>> obs.err  >> obs.txt
	./nexus foo2 12 2>> obs.err  >> obs.txt
	./nexus foo3 12 2>> obs.err  >> obs.txt
	./nexus foo4 12 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus pools __pycache__
