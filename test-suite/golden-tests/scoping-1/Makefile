all:
	rm -f *.err obs.txt
	morloc make -o nexus w.loc 2> build.err
	./nexus f > obs.txt 2> obs.err

clean:
	rm -f nexus
