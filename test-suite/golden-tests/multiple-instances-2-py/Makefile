all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus rms [2,4,4,8] > obs.txt 2> obs.err

clean:
	rm -rf nexus pools
