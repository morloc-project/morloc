all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus small 100 3 > obs.txt 2> obs.err

clean:
	rm -rf nexus pools *err __pycache__
