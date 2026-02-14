all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus foo 42 [1,2,3] > obs.txt 2> obs.err

clean:
	rm -f nexus *pdf
