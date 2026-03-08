all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus deep 1 2 3 4 5 6 7 8 > obs.txt 2> obs.err

clean:
	rm -rf nexus pools *err __pycache__
