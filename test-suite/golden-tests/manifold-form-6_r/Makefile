all:
	rm -f *.err obs.txt
	morloc make -o nexus t6.loc 2> build.err
	./nexus foo 16 > obs.txt 2> obs.err

clean:
	rm -f nexusR
