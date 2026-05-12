all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus foo 1000 100000 2>> obs.err  >> obs.txt
	./nexus foo 100000 1000 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus pools __pycache__
