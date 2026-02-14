all:
	rm -f *.err obs.txt
	morloc make -o nexus t2.loc 2> build.err
	./nexus foo > obs.txt 2> obs.err

clean:
	rm -f nexus
