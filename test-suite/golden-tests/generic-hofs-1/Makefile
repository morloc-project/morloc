all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus foo 2 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus pools
