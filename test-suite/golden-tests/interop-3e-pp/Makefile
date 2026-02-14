all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus foo 2 [1,2,3] [4,5,6] > obs.txt 2> obs.err

clean:
	rm -rf nexus pools *pdf
