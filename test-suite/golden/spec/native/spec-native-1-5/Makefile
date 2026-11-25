all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus sndReal '[3,2.75]' > obs.txt 2> obs.err
	./nexus sndStr '[3,"x"]' >> obs.txt 2>> obs.err
	./nexus fstStr '["y",2.75]' >> obs.txt 2>> obs.err
	./nexus sndGen '["a","z"]' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools
	rm -rf *-build *build.tmp*
	rm -f *.err
