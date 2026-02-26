all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus testSafeHead > obs.txt 2> obs.err
	./nexus testSafeHeadEmpty >> obs.txt 2>> obs.err
	./nexus testFromNull >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
