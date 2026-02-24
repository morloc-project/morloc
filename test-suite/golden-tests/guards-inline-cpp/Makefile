all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus classify 5 > obs.txt 2> obs.err
	./nexus classify 50 >> obs.txt 2>> obs.err
	./nexus classify 200 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
