all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus foo 1.5 2.5 > obs.txt 2> obs.err

clean:
	rm -rf nexus pools *err __pycache__
