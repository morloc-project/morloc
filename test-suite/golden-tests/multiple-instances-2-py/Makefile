all:
	rm -f *.err obs.txt
	morloc make -o nexus foo.loc 2> build.err
	./nexus rms [2,4,4,8] > obs.txt 2> obs.err

clean:
	rm -f nexus
