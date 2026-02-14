all:
	rm -f *.err obs.txt
	morloc make -o nexus foo.loc 2> build.err
	./nexus foo '[12345678,987654321]' 2>> obs.err  >> obs.txt

clean:
	rm -f nexus
