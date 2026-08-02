all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus foo '[9,9]' '[[1],[2,2]]' >> obs.txt 2> obs.err

clean:
	rm -rf nexus pools
