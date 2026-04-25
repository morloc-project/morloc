all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus fact 20 > obs.txt 2> obs.err
	./nexus fact 50 >> obs.txt 2>> obs.err
	./nexus fact 100 >> obs.txt 2>> obs.err
	./nexus fact 200 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
