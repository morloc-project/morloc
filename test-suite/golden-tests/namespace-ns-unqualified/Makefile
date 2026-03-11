all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc > obs.txt 2> build.err; grep -c "Undefined term" build.err > obs.txt 2> obs.err

clean:
	rm -rf nexus pools __pycache__
