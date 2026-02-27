all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus foo 3 2>> obs.err  >> obs.txt
	./nexus bar 3 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus pools
