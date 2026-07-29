all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus addI 3 4 >> obs.txt 2> obs.err
	./nexus meanReal '[1.0,2.0,3.0,4.0]' >> obs.txt 2>> obs.err
	./nexus minmax '[5,2,9,1]' >> obs.txt 2>> obs.err
	./nexus safeHead '[7,8]' >> obs.txt 2>> obs.err
	./nexus safeHead '[]' >> obs.txt 2>> obs.err
	./nexus --keep-null safeHead '[]' >> obs.txt 2>> obs.err
	./nexus selfCat '[1,2,3]' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools
