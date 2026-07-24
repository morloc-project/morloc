all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus sizeSingle 7 >> obs.txt 2>> obs.err
	./nexus sizeMany [3,5,8] >> obs.txt 2>> obs.err
	./nexus sizeConcrete >> obs.txt 2>> obs.err
	./nexus sizeAfterRoundTrip >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log
