all:
	rm -f *.err obs.txt
	morloc make -o nexus foo.loc 2> build.err
	bash run.sh > obs.txt 2> obs.err

clean:
	rm -rf nexus __pycache__ a.json b.json
