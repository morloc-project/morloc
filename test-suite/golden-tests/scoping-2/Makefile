all:
	rm -f *.err obs.txt
	morloc make -o nexus w.loc 2> build.err
	./nexus f 42 > obs.txt 2> obs.err

clean:
	rm -f nexus
