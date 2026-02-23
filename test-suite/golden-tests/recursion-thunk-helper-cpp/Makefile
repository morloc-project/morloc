all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus sumDown 0 > obs.txt 2> obs.err
	./nexus sumDown 1 >> obs.txt 2>> obs.err
	./nexus sumDown 3 >> obs.txt 2>> obs.err
	./nexus sumDown 5 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
