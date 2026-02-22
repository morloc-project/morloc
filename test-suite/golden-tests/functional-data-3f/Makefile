all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus foo true 3 > obs.txt 2> obs.err
	./nexus baz true 4 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus pools __pycache__
