all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus addInts 3 4 > obs.txt 2> obs.err
	./nexus addReals 1.5 2.5 >> obs.txt 2>> obs.err
	./nexus expr1 3 4 5 6 >> obs.txt 2>> obs.err
	./nexus expr2 2 3 4 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err __pycache__
