all:
	rm -f *.err obs.txt
	morloc make -o nexus foo.loc 2> build.err
	./nexus foo 2 2>> obs.err  >> obs.txt

clean:
	rm -f nexus
