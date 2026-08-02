all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus choose '[1,2,3]' '[4,5]' >> obs.txt 2> obs.err

clean:
	rm -rf nexus pools
