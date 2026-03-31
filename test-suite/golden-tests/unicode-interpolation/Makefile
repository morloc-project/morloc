all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	>obs.txt
	./nexus greet 2>> obs.err >> obs.txt
	./nexus multiByteInterp 2>> obs.err >> obs.txt

clean:
	rm -rf nexus pools __pycache__
