all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus isEven 0 > obs.txt 2> obs.err
	./nexus isEven 1 >> obs.txt 2>> obs.err
	./nexus isEven 4 >> obs.txt 2>> obs.err
	./nexus isOdd 0 >> obs.txt 2>> obs.err
	./nexus isOdd 3 >> obs.txt 2>> obs.err
	./nexus isOdd 4 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
