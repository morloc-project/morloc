all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus test 42 > obs.txt 2> obs.err

clean:
	rm -rf nexus pools __pycache__
