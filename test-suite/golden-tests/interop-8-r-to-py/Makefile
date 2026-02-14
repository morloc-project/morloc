all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus foo "true" > obs.txt 2> obs.err
	diff exp.txt obs.txt

clean:
	rm -rf nexus __pycache__
