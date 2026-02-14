all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus analyze -n 50 > obs.txt 2> obs.err
	./nexus summary 2>> obs.err  >> obs.txt
	./nexus groupBy -n 50 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus __pycache__
