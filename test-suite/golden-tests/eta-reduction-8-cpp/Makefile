all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus foo 23 35 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus __pycache__
