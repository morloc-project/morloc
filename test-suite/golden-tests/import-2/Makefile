all:
	rm -f *.err obs.txt
	morloc make -o nexus foo.loc 2> build.err
	./nexus person '"Bob"' 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus pools
